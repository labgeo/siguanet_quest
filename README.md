siguanet_quest
==============

PostgreSQL extension for [SIGUANET](https://github.com/labgeo/siguanet-dbsetup) geodatabases providing functions which return aggregate data based on spatial criteria.

## What's SIGUANET?
[SIGUANET](https://github.com/labgeo/siguanet-dbsetup) is a free software project that aims to share the University of Alicante's corporate built asset management technology ([SIGUA](http://www.sigua.ua.es)) with the developers community.
In this sense, [SIGUANET](https://github.com/labgeo/siguanet-dbsetup) will hopefully be useful for other universities and academic organizations.

## What's siguanet_quest
This is a set of plpgsql functions specifically designed for reporting information coming from a [SIGUANET](https://github.com/labgeo/siguanet-dbsetup) geodatabase at different levels of spatial aggregation.
Each function targets:
* a predefined level of spatial aggregation, ranging from the whole corporation down to a particular building floor
* an optional level of aggregation regarding organisational or use criteria
Function results basically return aggregated information on areas, number of rooms and number of employees.

### Database requirements
A working [SIGUANET](https://github.com/labgeo/siguanet-dbsetup) database is needed which meets the following requirements:
* PostgreSQL 9.2 or greater
* PostGIS 2.0 or greater

### Setting up
This is a PostgreSQL extension, so you'll need root access to your database server in order to copy the script and control files into the appropriate `SHAREDIR/extension` directory.
If your server has only one PostgreSQL installation, or you wish to place this extension on your first installation, things are easy:
just clone or download *siguanet_quest*, cd to the sources dir and execute:  
```shell
$ make install
```  
  
Once *siguanet_quest* is available at your PostgreSQL server, connect to your [SIGUANET](https://github.com/labgeo/siguanet-dbsetup) database and run:
```shell
# CREATE EXTENSION siguanet_quest;
```  
  
You should get a brand new schema named *quest* holding several hundreds of functions, as well as some views and types.  
You'll also find a table named *quest_adminroles* where administrators should add group roles which have authorisation for viewing unencrypted protected data.
Currently, personal identifiers (Spain's NIF) are the only data stored in [SIGUANET](https://github.com/labgeo/siguanet-dbsetup) databases which are considered protected.

## How does it work?
As said above, *siguanet_quest* is just a set of functions. Invoke them from *psql*, *pgAdmin* or any other client of your choice.
Function names are self-explanatory (in spanish, sorry guys!) but being familiar with the data structure of a [SIGUANET](https://github.com/labgeo/siguanet-dbsetup) database and having a look at the function code will help to clear any doubts about a particular function's purpose.
On the other hand, if you or your users need to take advantage of *siguanet_ques* on a more productive way, you may use [*siguanet-desktop*](https://github.com/labgeo/siguanet-desktop), a desktop application that provides a hierarchically organized interface to *siguanet_quest*.
