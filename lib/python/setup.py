#!/usr/bin/python

from distutils.core import setup, Extension

dolmod = Extension("dolmod", ["dolmod.c"],
                   libraries = ["dcui"])

setup(name = "dolcon-py",
      version = "1.1r1",
      description = "Python glue module for libdcui",
      author = "Fredrik Tolf",
      author_email = "fredrik@dolda2000.com",
      url = "http://www.dolda2000.com/~fredrik/doldaconnect/",
      ext_modules = [dolmod],
      packages = ["dolcon"],
      license = "GPL-2")
