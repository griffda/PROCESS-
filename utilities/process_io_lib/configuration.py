"""
PROCESS I/O library configuration API.

Designed to be used by utility developers when creating configuration
mechanisms for their tool.
"""

import json
import logging

# temporary
api_logger = logging.getLogger("process_api")


class ConfigurationParser(object):
    
    """Abstract parser class. Must be subclassed to be used.
    
    The parser should always put read-in data in the data property.
    """
    
    def __init__(self):
        self._data = None
        
    @property
    def data(self):
        return self._data
        
    @x.setter
    def data(self, value):
        """Validate the configuration is provided in a specific format."""
        assert isinstance(value, dict)
        self._data = value
    
    @x.deleter
    def data(self):
        del self._data


class JsonConfigParser(object):

    """JSON configuration parser."""    
    
    def __init__(self, filename):
        self.data = None
        try:
            with open(filename) as fh:
                self.data = json.load(fh)
        except FileNotFoundError:
            api_logger.error("Cannot find configuration file {}".format(filename))
            pass


class Config(object):
    
    """Generic configuration for PROCESS tools."""
    
    def __init__(self, config_file, parser=JsonConfigParser):
        self.config_file = config_file
        parser = JsonConfigParser(config_file)
        self.config_data = parser.data
        
    def _decomposed_value_of(self, config_param, *remaining):
        if not remaining:
            try:
                yield self.config_data[config_param]
            except KeyError:
                raise
        else:
            yield self.value_of(remaining[0], *remaining[1:])
    
    def value_of(self, config_param):
        config_path = config_param.split
        self._decomposed_value_of(config_path[0], config_path[1:])
        
    
    
    
    