#!/usr/bin/python

from distutils.core import setup, Extension

dolmod = Extension("dolmod", ["dolmod.c"],
                   libraries = ["dcui"])

setup(name = "dolcon",
      version = "0.3",
      description = "Python glue module for libdolcon",
      ext_modules = [dolmod],
      packages = ["dolcon"],
      license = "GPL-2")
