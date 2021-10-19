import datetime
import warnings
import os
import json
import pandas as pd
from bokeh.plotting import figure, output_file, save
from bokeh.models import ColumnDataSource

from process.io import mfile as mf


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
    # tracking_variables = {
    #     "pheat", "bootipf", "pinjmw", "shldith", "fwith", 
    #     "fwoth", "thshield", "tftsgap", "bore", "ohcth", 
    #     "scrapli", "blnkoth", "precomp", "tfthko", "blnkith", 
    #     "vvblgap", "scraplo", "gapoh", "gapsto", "shldoth", 
    #     "tfcth", "gapds", "pnucshld", "pnucblkt", "triang", 
    #     "triang95", "pcoreradmw", "tesep", "ralpne", "ieped", 
    #     "wallmw", "aspect", "rminor", "rmajor", "q95", "te", 
    #     "beta", "facoh", "zeff", "bt", "hfact", "kappa", "powfmw", 
    #     "teped", "powerht", "kappa95", "neped", "dene", "pradmw", 
    #     "ne0", "faccd", "dnz", "taueff", "te0", "pdivt", "nesep", 
    #     "vol", "sarea", "pnetelmw", "etath", "pgrossmw", "tftmp", 
    #     "n_tf", "bmaxtf", "vstot", "dnitot", "tburn", "divlife", 
    #     "step20", "step21", "step2101", "step2202", "step2203", 
    #     "step22", "step23", "step24", "step25", "cdirt", "concost"
    # }

    tracking_variables = {"pheat", "triang95"}


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
                warnings.warn(f'{var} is not present in the MFile and will be skipped.')
                continue
            
            self.tracking_file.meta[var] = data.get_scan(1)

        # tracking data
        for var in self.tracking_variables:
            data = self.mfile.data.get(var,DataDoesNotExist())

            if isinstance(data, DataDoesNotExist):
                continue

            if data.get_number_of_scans() > 1:
                warnings.warn(f'Only scan 1 will be tracked, but {var} has {data.get_number_of_scans()} scans.')
            
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

    figures = []

    for k, v in data.tracked_variables.items():
        df = v.as_dataframe()
        source = ColumnDataSource(df)
        fig = figure()
        fig.scatter(x='date', y='value', source=source)
        figures.append(fig)

        output_file(filename=f'{k}.html')
        save(fig)
        





if __name__ == '__main__':
    x = ProcessTrackerGenerator('/root/process/tracking/baseline_2018/baseline_2018_MFILE.DAT', '/root/process/tracking/db/')
    x = ProcessTrackerGenerator('/root/process/tracking/baseline_2018/ref_baseline_2018_MFILE.DAT', '/root/process/tracking/db/')

    plot_tracking_data('/root/process/tracking/db/')