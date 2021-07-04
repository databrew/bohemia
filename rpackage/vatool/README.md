
<!-- README.md is generated from README.Rmd. Please edit that file -->

# VA Tool

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

## Installation

To install from github, run:

    devtools::install_github('databrew/bohemia', subdir = 'rpackage/bohemia')

## Credentials

Save a `credentials/credentials.yaml` in `dev`.

Save a `credentials/vatoolusers.csv` in `dev`.

## Setting up ICD-10 codes

Run `create_data.R` in `data-raw`. This will create two r data objects
named `icd_data_moz` and `icd_data_tza`.

## Setting up back-end

You’ll need to set up a back-end. In order to this, create a postgres
database named `bohemia`:

    psql
    drop database vadb;   
    create database vadb;
    exit

Then go into the psql cli in bohemia:

    psql vadb

and copy-paste the code from `set_up_database.sql`

Then, in order to create fake data, run:

    Rscript create_fake_data.R

## Development

To run locally, run:

    cd dev
    Rscript run_dev.R

## Deploying database to AWS

-   console.aws.amazon.com/rds/
-   Click “Create database”
-   Click “PostgreSQL 12.5-R1”
-   Click “Production”
-   Set db cluster identifier to “vatool”
-   Set password
-   Set instance class to db.t3.micro
-   Set default vpc
-   Make public
-   For security, use bohemiadbgroup
-   Set initial database name to “vatool”
