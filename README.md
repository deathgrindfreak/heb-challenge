# HEB Code Challenge

## Setup
Create and setup the DB by running the `Makefile`

``` sh
make
```

Then run the project with

``` sh
stack run
```
Or alternately

``` sh
stack build
```
Followed by

``` sh
stack exec heb-challenge
```

## Configuration
All of the configuration for the server lives in [config.dhall](./config.dhall)

The config expects two files, `imagga_key` and `imagga_secret` which can be generated on the [Imagga website](https://imagga.com)

## API
> get `/images` - Retrieve all image metadata
```sh
http localhost:3000/images
```

> get `/images?objects="foo,bar,quux"` - Retrieve all image metadata with tags "foo", "bar" and "quux"
```sh
http localhost:3000/images?objects="foo,bar,quux"
```

> get `/images/:id` - Retrieve image metadata for a specific image
```sh
http localhost:3000/images/ce3817a0-1b24-4586-b7fb-a64b4b98b717
```

> post `/images` - Inserts an image along with its metadata

Form fields:
- discover - Turn on image discovery by shipping the image off to Imagga
- label - A label for the image
- url - The url for the image
```sh
http --form POST localhost:3000/images \
  discover="true" \
  label="A Label" \
  url="https://imagga.com/static/images/tagging/wind-farm-538576_640.jpg"

```
