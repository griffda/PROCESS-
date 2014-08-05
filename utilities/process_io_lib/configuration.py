"""
PROCESS I/O library configuration API.

Designed to be used by utility developers when creating configuration
mechanisms for their tool.
"""

import json
import logging

api_logger = logging.getLogger("process_api")


class ConfigurationParser(object):
    
    """Abstract parser class. Must be subclassed to be used."""
    
    pass


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
        
    def 
    
    
    
    