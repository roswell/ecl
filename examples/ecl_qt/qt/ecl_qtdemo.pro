#-------------------------------------------------
#
# Project created by QtCreator 2016-08-10T18:00:40
#
#-------------------------------------------------

QT       += core gui

CONFIG+=c++14
greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = ecl_qtdemo
TEMPLATE = app


SOURCES += main.cpp\
        hybrid_main.cpp \
    cl_bridge_utils.cpp

HEADERS  += hybrid_main.h \
    cl_bridge_utils.hpp

FORMS    += hybrid_main.ui

# The include path that contains ecl/ecl.h
QMAKE_CFLAGS += `ecl-config --cflags`
QMAKE_CXXFLAGS += `ecl-config --cflags`

# The ECL shared library directory.
QMAKE_LFLAGS += `ecl-config --ldflags` -lecl

# Lisp library written by a user
LIBS += $$_PRO_FILE_PWD_/lisp-envi.a


RESOURCES += \
    resource.qrc

