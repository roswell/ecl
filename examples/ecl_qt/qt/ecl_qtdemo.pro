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

#The include path that contains ecl/ecl.h
INCLUDEPATH += /usr/local/include
#The ECL shared library directory.
LIBS  += /usr/local/lib/libecl.dylib

LIBS += $$_PRO_FILE_PWD_/../lisp-envi.a


RESOURCES += \
    resource.qrc

