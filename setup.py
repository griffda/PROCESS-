from setuptools import setup

setup_kwargs = {
    "name": "process",
    "version": "1.0.17",
    "description": ("Power Reactor Optimisation Code for Environmental and "
        "Safety Studies"),
    "url": "https://ccfe.ukaea.uk/resources/process/",
    "author": "UKAEA",
    "packages": ["utilities"],
    "entry_points": {
        "console_scripts": [
            "process=utilities.process:main"
        ]
    }
}

if __name__ == "__main__":
    setup(**setup_kwargs)