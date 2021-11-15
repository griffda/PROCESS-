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
    """Acts as the data storage for a given JSON tracking file"""
    def __init__(self) -> None:
        self.meta: dict = {}
        """metadata (e.g. commit message and date generated) as a key-value pair"""
        
        self.tracking: dict = {}
        """tracking data that shows the value of an important variable as a key-value pair"""
    
    def asdict(self) -> dict:
        return {'meta': self.meta, 'tracking': self.tracking}


class DataDoesNotExist:
    pass


class ProcessTracker:
    """Manages the creation of tracking data into JSON files."""

    meta_variables = {"commsg", "date", "time"}
    """Variables in an MFile that hold metadata we want to show on the graph"""

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
    """Variable in an MFile that we will track the changes in.
    
    1) A variable of the form X.Y shows that variable Y exists as a member of module Y. This is needed for variables no longer in Fortran modules, where their parent cannot be scraped.
    2) Other variables should be a module variable for a wrapped module. The name of the parent module will be automatically determined.
    """


    def __init__(self, mfile: str, database: str = None) -> None:
        """Drive the creation of tracking JSON files.

        :param mfile: the path to an mfile to create tracking data for.
        :type mfile: str

        :param database: the folder (acting as a database) that stores all tracking JSON files
        :type database: str
        """
        self.mfile = mf.MFile(mfile)
        self.tracking_file = TrackingFile()

        self._generate_data()

        title = os.path.splitext(os.path.basename(mfile))[0]
        self.add_extra_metadata('title', title)

        if database:
            date = self.tracking_file.meta.get('date').replace('/','')
            time = self.tracking_file.meta.get('time')

            # for an mfile called foo.MFILE.DAT run 16:00 on 15/11/2021
            # the tracking data file will be written to 
            # foo_MFILE-15112021-16:00.json
            tracking_file_name = os.path.join(database, f'{title}-{date}-{time}.json')

            self.write(tracking_file_name)
    

    def add_extra_metadata(self, key, value):
        """Enables the adding of extra metadata of key: value"""
        self.tracking_file.meta[key] = value
    

    def _generate_data(self):
        """
        Generates metadata for all metadata variables in ProcessTracker.meta_variables
        Generates tracking data for all variables in ProcessTracker.tracking_variables.
        """
        # meta data
        for var in self.meta_variables:

            try:
                # value of var in the mfile
                variable_data = self.mfile.data[var]
            except:
                print(f'{var} is not present in the MFile and will be skipped.')
                continue

            
            self.tracking_file.meta[var] = variable_data.get_scan(1)

        # tracking data
        for var in self.tracking_variables:
            if '.' in var:
                # a dotted variable is for variables that no longer exist in Fortran module variables
                # see tracking_variables docstring
                try:
                    _, var = var.split('.')
                except:
                    print(f'{var} is a dotted variable and must be in the form OVERRIDINGNAME.VARIABLE')

            try:
                # value of the variable extracted from the mfile
                mfile_var_value = self.mfile.data[var]
            except:
                print(f'{var} is not present in the MFile and will be skipped.')
                continue
                

            if mfile_var_value.get_number_of_scans() > 1:
                print(f'Only scan 1 will be tracked, but {var} has {mfile_var_value.get_number_of_scans()} scans.')
            
            self.tracking_file.tracking[var] = mfile_var_value.get_scan(1)
    

    def write(self, filename: str):
        """Write the metadata and tracking data into the JSON file"""
        with open(filename, 'w') as f:
            json.dump(self.tracking_file.asdict(), f, indent=4)


class TrackedVariable:
    """Represents a graph for variable `name`. Tuples (points) with the same title are plotted as part of the same line"""
    def __init__(self, name: str) -> None:
        self.name = name
        """Name of the graph this variable is plotted under"""
        self._data = []
        """Tuple of (title, timestamp, data, message)
        
        title: name of the run e.g. starfire or baseline_2018 (not unique)
        timestamp: date of the specific run this tuple refers to (unique)
        data: the value of the variable, `name`, on `title` run at time `date` (possibly unique)
        message: latest commit message when `title` was run at time `date` (possibly unique)
        """


    def add_datapoint(self, title, data, message, timestamp):
        """
        Adds a tuple to this graph

        title: name of the run e.g. starfire or baseline_2018 (not unique)
        timestamp: date of the specific run this tuple refers to (unique)
        data: the value of the variable, `name`, on `title` run at time `date` (possibly unique)
        message: latest commit message when `title` was run at time `date` (possibly unique)
        """
        self._data.append((title, timestamp, data, message))
    

    def as_dataframe(self):
        """
        Converts our internal data representation of this graph as a dataframe.

        title -> title
        timestamp -> date
        data -> value
        message -> annotation
        """
        df = pd.DataFrame(self._data)
        df.columns = ('title', 'date', 'value', 'annotation')

        return df


class TrackedData:
    def __init__(self, database) -> None:
        self.database = database

        self.tracked_variables = {}

        self._track()


    def _add_variables(self, data):
        """Adds the `data` of an entire JSON tracking file to an internal store before being transformed into a dataframe, to then be plotted."""

        # extract the metadata from our file as all datapoints of this file will require them
        metadata = data['meta']
        title = metadata.get('title')
        message = metadata.get('commsg')
        date_str = metadata.get('date')
        time_str = metadata.get('time')

        # common format for the timestamp of the run
        data_time_str = f'{date_str.strip()} - {time_str.strip()}'
        date_time = datetime.datetime.strptime(data_time_str, '%d/%m/%Y - %H:%M')
        
        tracking_data = data.get('tracking', [])
        for k, v in tracking_data.items():
            # create a TrackedVariable when we see a variable we do not know
            if not k in self.tracked_variables.keys():
                self.tracked_variables[k] = TrackedVariable(k) 
            
            self.tracked_variables.get(k).add_datapoint(title, v, message, date_time)
            

    def _track(self):
        # open all files in the `database` folder
        files = [os.path.join(self.database, i) for i in os.listdir(self.database) if os.path.isfile(os.path.join(self.database, i))]

        for i in files:
            with open(i, 'r') as f:
                file_data = json.load(f) # parsed contents of the JSON tracking file
                self._add_variables(file_data) # add all the data in this JSON file to our internal store


def plot_tracking_data(database):
    """
    Drives the processing of existing .json tracking files and then plotting of this processed data.
    """
    data = TrackedData(database)

    figures = {}

    overrides = {}

    for i in ProcessTracker.tracking_variables:
        try:
            overriden_name, variable = i.split('.')
            overrides[variable] = overriden_name
        except:
            continue

    for k, v in data.tracked_variables.items():
        df = v.as_dataframe() # all the data for one tracked variable as a dataframe

        # overrides trumps fortran scrapping
        parent = overrides.get(k) or PythonFortranInterfaceVariables.parent_module(k) # module name (or given name if overridden)

        if not parent:
            print(f'Variable {k} is not a module variable of any Fortran module, please provide the python class which this variable can be found under as CLASS.VARIABLE noting that VARIABLE must be a variable present in the output file and does not necessarily need to correspond to an actual class variable, VARIABLE, of CLASS')
            continue


        if figures.get(parent) is None:
            figures[parent] = []
        
        titles = list(df['title'].to_numpy()) # all scenarios this variable is tracked in

        figur = figure(title=k, 
            x_axis_type='datetime',
            plot_width=600,
            plot_height=600
        )

        colours = itertools.cycle(Bokeh[8]) # hardcode at 8 because no 2
        # each title (different scenario) has a different line colour
        for t in titles:
            subdf = df[df['title'] == t]
            subsource = ColumnDataSource(subdf) # convert dataframe into Bokeh compatible
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

        figur.legend.click_policy="hide" # hide a line if its legend entry is clicked

        # show a title, commit, and date when hovering over a datapoint
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

    # each module/overriden name e.g. CostsStep has a panel which holds graphs for all variables under that scope
    for grp, fig in figures.items():
        gplot = gridplot([fig[i:i+2] for i in range(0, len(fig), 2)])
        panels.append(Panel(child=gplot, title=grp))
    

    tabs = Tabs(tabs=panels)
    
    return file_html(tabs, CDN, 'PROCESS Regression Testing Visualisation')


def write_tracking_html_file(database, output):
    """Writes the visual tracking data to an appropriate file"""
    tracking_html = plot_tracking_data(database)

    with open(output, 'w') as f:
        f.write(tracking_html)


class PythonFortranInterfaceVariables:
    """
    Parses the f2py generated tree and can find the parent module of a module variable.
    """

    tree = {}
    
    @classmethod
    def populate_classes(cls):
        """
        Scrape all the data into an internal data storage
        """
        classes = {}

        for name, module in inspect.getmembers(fortran):
            if type(module) == type(fortran.main_module):
                classes[name] = cls._get_variables(module)
        
        cls.tree = classes


    @classmethod
    def _get_variables(cls, fortran_module):
        """
        Get the variables of a given module
        """
        functions = []

        for name, function in inspect.getmembers(fortran_module):
            if type(function) != type(fortran.main_module.inform):
                functions.append(name)
        
        return functions
    

    @classmethod
    def parent_module(cls, var: str):
        """
        Return the parent module of var, or None.
        """
        if not cls.tree:
            cls.populate_classes()

        for mod, functions in cls.tree.items():
            if var in functions:
                return mod
        
        return None


def track_entrypoint(arguments):
    """
    Entrypoint if we run in track mode.

    Generates a tracking JSOn file for the provided MFile.
    """
    if not arguments.db or not arguments.mfile:
       raise ValueError('track requires --db and --mfile be set')

    ProcessTracker(mfile=arguments.mfile, database=arguments.db)


def plot_entrypoint(arguments):
    """
    Entrypoint if we run in plot mode.

    Plots all tracking data into a single tracking.html file
    """
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