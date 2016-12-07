#!/bin/bash

f2py -h --overwrite-signature rhosigchi_f2py.pyf -m rhosigchi_f2py rhosigchi_f2py.f

f2py -c rhosigchi_f2py.pyf rhosigchi_f2py.f