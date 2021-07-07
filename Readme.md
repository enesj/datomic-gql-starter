# datomic-graphql-starter

#### Introduction

This project is intended to become a Clojure library that scaffolds GraphQL schema and resolvers, with queries and mutations working out of the box, based on an existing [Datomic](https://www.datomic.com/) db schema.

The purpose of this (future) library is to auto-generate the repetitive boilerplate needed for GraphQL endpoint based on famous [Lacinia](https://github.com/walmartlabs/lacinia) and [Pedestal](http://pedestal.io/) libraries, and the idea had come from the library called [Stillsuit](https://github.com/workframers/stillsuit) that already had implemented foundation code for this task under Apache license.

Stillsuit has to be used in conjunction with another library made by the same people from [Workframe](https://workframe.com) called [Catchpocket](https://github.com/workframers/catchpocket) and requires manual preparation of the config file. Schema produced with Stillsuit had only some basic queries and no mutations at all. It also has limitations to work only with Datomic peer API (no Client API support).

I wanted to make a fully automated and customizable library that works on both client and peer Datomic API. To achieve this I've decided to incorporate Stillsuit and Catchpocket code in my project as to be able to modify their code as needed.

At the same time, I was actively searching the internet for a similar kind of library (not necessarily for Datomic) to see what is possible and desirable. Recently I've stumbled upon the library called [Mongo-graphql-starter](https://github.com/arackaf/mongo-graphql-starter) that has implemented a lot of automation I was thinking of.

My goal would be to implement as much as possible of this library has already implemented. Also, this library has helped me to make my decision about the name of the project.

#### Installation

This project is intended to be used/tested with Datomic Pro Starter with Peer or Client API-s. I am using and *datomic-pro-0.9.5703* versions.

Datomic Pro Starter from [this link](https://my.datomic.com/downloads/pro). After downloading, unzip it at any place that is convenient for you.


###### Running Datomic Pro/Datomic Pro Starter

**Datomic Pro Starter** is the free version with all features of Datomic Pro but is limited to *1 year of maintenance and updates*. **Datomic Pro Starter** supports both Datomic **Peer API** and **Client API**. 

Before running Datomic Pro Starter transactor you will need to:

- copy ./config/samples/dev-transactor-template.properties to the root of folder where your Datomic installation resides. 
- modify ./dev-transactor-template.properties file by inserting your **license key** at the beginning of this file (license-key= tag). You can get your License key from my.datomic.com by mail when you press **"Send License Key"** button.
- Install Datomic Pro locally using **bin/maven-install** from the installation folder
- After that you can run Datomic transactor with:

`bin/transactor dev-transactor-template.properties`

That's enough to be able to communicate with db using Datomic Peer API.

If you want to communicate with DB using Datomic Client API it's needed to run Datomic Peer Server with command like this:

`bin/run -m datomic.peer-server -h localhost -p 8998 -a "access-key","secret" -d mbrainz,datomic:dev://localhost:4334/mbrainz`

Of course, you should replace strings "access-key" and "secret" with something you use in your system.

###### Import Mbrainz sample database

For testing purposes, I am using [Datomic MusicBrainz sample database](https://github.com/Datomic/mbrainz-sample) that is a subset of the [Mbrainz](http://musicbrainz.org/) database covering the period 1968-1973. You can download the backup of this database from [this location](https://s3.amazonaws.com/mbrainz/datomic-mbrainz-1968-1973-backup-2017-07-20.tar).

Then you will unzip it in folder and restore it to your local Datomic Pro instance (first run Datomic Pro transactor in terminal) with this command:

`bin/datomic restore-db file:///full-path/to/backup/mbrainz datomic:dev://localhost:4334/mbrainz`

**Windows users** should specify a [basis-t](https://docs.datomic.com/on-prem/clojure/index.html#datomic.api/basis-t) to restore, like this:

`bin/datomic restore-db file:///path/to/backup/mbrainz datomic:dev://localhost:4334/mbrainz 148348`

In examples we use, we've renamed database "mbrainz-1968-1973" for simplicity to "mbrainz".

Finally, you should clone/download/fork [this project](https://github.com/enesj/datomic-gql-starter).

#### Datomic-graphql-starter configuration files

###### config.fern

Basic project configuration file is `config.fern` in root project folder:

```clojure
{
;full paths
 api-conf               (fern/lit concat @root @api-conf-file)
 stillsuit-conf         (fern/lit concat @root @stillsuit-conf-file)
 catchpocket-conf       (fern/lit concat @root @catchpocket-conf-file)
 refs-conf              (fern/lit concat @root @refs-conf-file)

;root
 root                   (fern/lit concat @config-root @db-name "/")

; datomic db
 db-name                "mbrainz"
 secret "enes"
 db-dev                     (fern/lit concat "datomic:dev://localhost:4334/" @db-name)
 db-free                    (fern/lit concat "datomic:free://localhost:4334/" @db-name)

;files
 api-conf-file          "api-config.edn"
 refs-conf-file         "refs-config.edn"
 catchpocket-conf-file  "catchpocket-config.edn"
 stillsuit-conf-file    "stillsuit-config.edn"

;paths
  config-root            "resources/config/"
 }
```

`config.fern` file is written in [fern](https://github.com/cognitect-labs/fern) format and defines names and locations for other configuration files, current database name and secret, Datomic Free and Dev database `uris`. This is the only config file that should be created manually .

Normally only fields here that needs to be changed  are `db-name` and `secret`,  but you can experiment with others too.

`db-name`  *defines the name of database we are connecting to with Graphql server*

`secret` *is the secret key that we use in script that runs Datomic Peer Server*

Other configuration files will be created automatically in `resources/config/<db-name>` folder when we run project from REPL.

###### api-config.edn

This file contains map with five keys:

`:entities`

`:queries`

`:inserts`

`:deletions`

`:updates`



For **Mbrainz DB** without manual editing it looks like this:

```clojure
{:entities
 ["country"
  "artist"
  "language"
  "release"
  "medium"
  "label"
  "abstractRelease"
  "track"
  "script"],
 :queries [],
 :inserts []
 :deletions []
 :updates []}
```



###### catchpocket-config.edn

Depends on `refs-config.edn`. If delete this file it will be recreated together and then `stillsuit-congfig.edn` will be recreated.

###### stillsuit-congfig.edn

Depends on `catchpocket-config.edn`













