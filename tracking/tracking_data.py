import datetime
import os
import json
import itertools
import pandas as pd
import inspect
import argparse
import re
from bokeh.plotting import figure
from bokeh.models import ColumnDataSource, HoverTool
from bokeh.layouts import gridplot
from bokeh.models.widgets import Panel, Tabs
from bokeh.palettes import Bokeh
from bokeh.resources import CDN
from bokeh.embed import file_html

from process.io import mfile as mf
from process import fortran


class TrackingFile:
    def __init__(self) -> None:
        self.meta: dict = {}
        self.tracking: dict = {}
    
    def asdict(self) -> dict:
        return {'meta': self.meta, 'tracking': self.tracking}


class DataDoesNotExist:
    pass


class ProcessTrackerGenerator:
    meta_variables = {"commsg", "date", "time"}
    tracking_variables = {
        "pheat", "bootipf", "pinjmw", "shldith", "fwith", 
        "fwoth", "thshield", "tftsgap", "bore", "ohcth", 
        "scrapli", "blnkoth", "precomp", "tfthko", "blnkith", 
        "vvblgap", "scraplo", "gapoh", "gapsto", "shldoth", 
        "tfcth", "gapds", "pnucshld", "pnucblkt", "triang", 
        "triang95", "pcoreradmw", "tesep", "ralpne", "ieped", 
        "wallmw", "aspect", "rminor", "rmajor", "q95", "te", 
        "beta", "facoh", "zeff", "bt", "hfact", "kappa", "powfmw", 
        "teped", "powerht", "kappa95", "neped", "dene", "pradmw", 
        "ne0", "faccd", "dnz", "taueff", "te0", "pdivt", "nesep", 
        "vol", "sarea", "pnetelmw", "etath", "pgrossmw", "tftmp", 
        "n_tf", "bmaxtf", "vstot", "dnitot", "tburn", "divlife", 
        "CostsStep.step20", "CostsStep.step21", "CostsStep.step2101", 
        "CostsStep.step2202", "CostsStep.step2203", "CostsStep.step22", 
        "CostsStep.step23", "CostsStep.step24", "CostsStep.step25", "cdirt", "concost"
    }


    def __init__(self, mfile: str, database: str = None) -> None:
        self.mfile = mf.MFile(mfile)
        self.tracking_file = TrackingFile()

        self._generate_data()

        title = os.path.splitext(os.path.basename(mfile))[0]
        self.add_extra_metadata('title', title)

        if database:
            date = self.tracking_file.meta.get('date').replace('/','')
            time = self.tracking_file.meta.get('time')
            path = os.path.join(database, f'{title}-{date}-{time}.json')

            self.write(path)
    

    def add_extra_metadata(self, key, value):
        self.tracking_file.meta[key] = value
    

    def _generate_data(self):
        # meta data
        for var in self.meta_variables:
            data = self.mfile.data.get(var,DataDoesNotExist())

            if isinstance(data, DataDoesNotExist):
                print(f'{var} is not present in the MFile and will be skipped.')
                continue
            
            self.tracking_file.meta[var] = data.get_scan(1)

        # tracking data
        for var in self.tracking_variables:
            data = self.mfile.data.get(var,DataDoesNotExist())

            if isinstance(data, DataDoesNotExist):
                continue

            if data.get_number_of_scans() > 1:
                print(f'Only scan 1 will be tracked, but {var} has {data.get_number_of_scans()} scans.')
            
            self.tracking_file.tracking[var] = data.get_scan(1)
    

    def write(self, filename: str):
        with open(filename, 'w') as f:
            json.dump(self.tracking_file.asdict(), f, indent=4)


class TrackedVariable:
    def __init__(self, name: str) -> None:
        self.name = name
        self._data = []


    def add_datapoint(self, title, data, message, timestamp):
        self._data.append((title, timestamp, data, message))
    

    def as_dataframe(self):
        df = pd.DataFrame(self._data)
        df.columns = ('title', 'date', 'value', 'annotation')

        return df


class TrackedData:
    def __init__(self, database) -> None:
        self.database = database

        self.tracked_variables = {}

        self._track()


    def _add_variables(self, data):
        metadata = data['meta']
        title = metadata.get('title')
        message = metadata.get('commsg')
        date_str = metadata.get('date')
        time_str = metadata.get('time')

        data_time_str = f'{date_str.strip()} - {time_str.strip()}'
        date_time = datetime.datetime.strptime(data_time_str, '%d/%m/%Y - %H:%M')
        
        tracking_data = data.get('tracking', [])
        for k, v in tracking_data.items():
            if not k in self.tracked_variables.keys():
                self.tracked_variables[k] = TrackedVariable(k) 
            
            self.tracked_variables.get(k).add_datapoint(title, v, message, date_time)
            

    def _track(self):
        files = [os.path.join(self.database, i) for i in os.listdir(self.database) if os.path.isfile(os.path.join(self.database, i))]

        for i in files:
            with open(i, 'r') as f:
                data = json.load(f)
                self._add_variables(data)


def plot_tracking_data(database):
    data = TrackedData(database)

    figures = {}

    overrides = {}

    for i in ProcessTrackerGenerator.tracking_variables:
        try:
            overriden_name, variable = i.split('.')
            overrides[variable] = overriden_name
        except:
            continue

    for k, v in data.tracked_variables.items():
        df = v.as_dataframe()

        parent = overrides.get(k) or PythonFortranInterfaceVariables.parent_module(k)

        if not parent:
            print(f'Variable {k} is not a module variable of any Fortran module, please provide the python class which this variable can be found under as CLASS.VARIABLE noting that VARIABLE must be a variable present in the output file and does not necessarily need to correspond to an actual class variable, VARIABLE, of CLASS')
            continue


        if figures.get(parent) is None:
            figures[parent] = []
        
        titles = list(df['title'].to_numpy())

        figur = figure(title=k, 
            x_axis_type='datetime',
            plot_width=600,
            plot_height=600
        )

        colours = itertools.cycle(Bokeh[8]) # hardcode at 8 because no 2

        for t in titles:
            subdf = df[df['title'] == t]
            subsource = ColumnDataSource(subdf)
            colour = next(colours)

            figur.scatter(x='date', 
            y='value', 
            source=subsource, 
            legend_label=t,
            color=colour
        )

            figur.line(x='date', 
            y='value', 
            source=subsource, 
            legend_label=t,
            color=colour,
            line_color=colour)

        figur.legend.click_policy="hide"

        hovertool = HoverTool(
            tooltips=[
                ('Title', '@title'),
                ('Commit', '@annotation'),
                ('Date', '@date{%Y-%m-%d %H:%M}'),
            ],
            formatters={
                '@title': 'printf',
                '@date': 'datetime',
                '@annotation': 'printf',
            },
        )

        figur.add_tools(hovertool)

        figures[parent].append(figur)
    
    panels = []

    for grp, fig in figures.items():
        gplot = gridplot([fig[i:i+2] for i in range(0, len(fig), 2)])
        panels.append(Panel(child=gplot, title=grp))
    

    tabs = Tabs(tabs=panels)
    
    return file_html(tabs, CDN, 'PROCESS Regression Testing Visualisation')


def write_tracking_html_file(database, output):
    tracking_html = plot_tracking_data(database)

    with open(output, 'w') as f:
        f.write(tracking_html)


class PythonFortranInterfaceVariables:

    tree = {}
    
    @classmethod
    def populate_classes(cls):
        classes = {}

        for name, module in inspect.getmembers(fortran):
            if type(module) == type(fortran.main_module):
                classes[name] = cls._get_variables(module)
        
        cls.tree = classes


    @classmethod
    def _get_variables(cls, fortran_module):
        functions = []

        for name, function in inspect.getmembers(fortran_module):
            if type(function) != type(fortran.main_module.inform):
                functions.append(name)
        
        return functions
    

    @classmethod
    def parent_module(cls, var: str):
        if not cls.tree:
            cls.populate_classes()

        for mod, functions in cls.tree.items():
            if var in functions:
                return mod
        
        return None


def track_entrypoint(arguments):
    if not arguments.db or not arguments.mfile:
       raise ValueError('track requires --db and --mfile be set')

    ProcessTrackerGenerator(mfile=arguments.mfile, database=arguments.db)


def plot_entrypoint(arguments):
    if not arguments.db or not arguments.out:
       raise ValueError('plot requires --db and --out be set')

    write_tracking_html_file(database=arguments.db, output=arguments.out)

if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    parser.add_argument('mode', type=str, choices=['track', 'plot'])

    parser.add_argument('-d','--db', type=str, default=None)
    parser.add_argument('-o','--out', type=str, default=None)
    parser.add_argument('-m','--mfile', type=str, default=None)

    arguments = parser.parse_args()

    if arguments.mode == 'track':
        track_entrypoint(arguments)
    elif arguments.mode == 'plot':
        plot_entrypoint(arguments)