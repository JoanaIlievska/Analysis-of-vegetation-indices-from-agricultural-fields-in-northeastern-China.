---title:"Analiza na vegetaciskite indeksi od polinja so posevi vo severoistocna Kina"
author:"Fang, Hongliang"
date:"`r Sys.Date(2021)`"
output:rmarkdown::html_vignette
vignette:>
  %\VignetteIndexEntry{
    Vegetation structural field measurement data  Northeastern China Crops
  }
%\VignetteEngine{
  knitr::rmarkdown
}
%\VignetteEncoding{
  UTF - 8
}
---```{
  r include = FALSE
}
has_pandoc <-  rmarkdown::pandoc_available()
```


```{
  r
}
library(EML)
library(emld)
```

#Here we construct a common EML file, including:

#  - Constructing more complete lists of authors, publishers and contact.
#- Summarizing the geographic, temporal, and taxonomic coverage of the dataset
#- Reading in pages of methods descriptions from a Word document
#- Adding arbitrary additional metadata
#- Indicating the canonical citation to the paper that should be acknowledged when the data is re-used.
#- Conversion between EML and other metadata formats, such as NCBII and ISO standards.

#In so doing we will take a modular approach that will allow us to build up our metadata from reusable components, while also providing a more fine-grained control over the resulting output fields and files.

### Overview of the EML hierarchy

#A basic knowledge of the components of an EML metadata file is essential to being able to take full advantage of the language.
#While more complete information can be found in the
#[official schema documentation](https://knb.ecoinformatics.org/#external//emlparser/docs/eml-2.1.1/index.html), here we provide a
#general overview of commonly used metadata elements most relevant to describing data tables.

This schematic shows each of the metadata elements we will generate.  Most these elements have sub -
  components (e.g. a 'publisher' may have a name, address, and so forth) which are not shown for simplicity.  Other optional fields we will not be generating in this example are also not shown.

```yaml
- eml
- dataset
- creator
- title
- publisher
- pubDate
- keywords
- abstract
- intellectualRights
- contact
- methods
- coverage
- geographicCoverage
- temporalCoverage
- taxonomicCoverage
- dataTable
- entityName
- entityDescription
- physical
- attributeList

```



### Authors and links

In this example, we will use R to re -
  generate
the EML metadata originally published by [Fang H,
                                          (2021)](https: /
                                                    / doi.pangaea.de / 10.1594 / PANGAEA.939444)
through the Gang H Long Term Ecological Research
Center, accompanying the PNAS paper [Yinghui , (2018)](https: /
                                                         / doi.org / 10.1594 / PANGAEA.900090). We have made only
a few modifications to simplify the presentation of this tutorial,
so our resulting EML will not be perfectly identical to the original.


### Our strategy

We will build this EML file from the bottom up, starting with the two main components
of a `dataTable` indicated above:the `attributeList` and the `physical` file type.
We will then slip these two pieces into place inside a `dataTable` element, and slip
that into our `eml` element along with the rest of the generic metadata, much like
building a puzzle or nesting a set of Russian dolls.

The original metadata file was created in association with the publication in PNAS
based on a Microsoft Word document template that Harvard Forest provides
to the academic researchers.  Metadata from this template is then read off
by hand and an EML file is generated using a combination of a commercial
XML editing platform (Oxygen) for commonly used higher -
  level elements,
and the Java platform `Morpho` provided by the EML development team for lower level attribute metadata.


## Attribute Metadata

A fundamental part of EML metadata is a description of the attributes
(usually columns) of a text file (usually a csv file) containing the
data being described.  This is the heart of many EML files.

```{
  r
}
attributes <-
  tibble::tribble(
    ~ attributeName,              ~ attributeDefinition,                             ~ formatString,             ~ definition,        ~ unit,  ~ numberType,
    "method",     "Metod koj e vnesen kako priroden broj vo opseg:1-5 ",                 NA,       "reden broj na refenciraniot metod",  NA,       NA,
    "Datum",                "Datum na sobirawe podatoci",                            "YYYY/MM/DD",                     NA,               NA,       NA,
    "latitude",    "Geografski koordinati mereni od istok-zapad po prviot meridijan.","cel_del.dec_del",        "koordinati",          "DMS",    "realen",
    "mean","sredna vrednost na vegetaciskiot indeks",                                  "cel.decimalen",               NA,                NA,     "realen",
    "std","standardna devijacija na dadeniot indeks",                                        NA,"        otstapuvanje od standardite",   NA,     "realen",
    "vegetaciski.indeks"," ime na vegetaciskiot indeks ",                                    NA,         "ime- znacenje na indeks",      NA,        NA,
    "longitude", "geografski koordinati sever-jug spored ekvator",                    "cel_del.dec_del",           NA,                  "DMS",      NA,
    "name ","Ime na povrshinata kade se naogja poleto ",                                   "string",               NA,                  NA,         NA,
    "reference",                  "referenca",                                              NA,                    NA,                  NA,         NA,
    "crop.type","Vidot na rastenie na zasegnatite polinja",                               "Ime",                   NA,                  NA,         NA
  )


```

Every column (attribute) in the dataset needs an `attributeName` (column name, as it appears in the CSV file) and `attributeDefinition`, a longer description of what the column contains. Additional information required depends on the data type:**
  Strings ** (character vectors) data just needs a "definition" value, often the same as the `attributeDefinition` in this case.

**
  Numeric ** data needs a `numberType` (e.g. "real", "integer"), and a unit.

**
  Dates ** need a date format.

**
  Factors ** (enumerated domains) need to specify definitions for each of the code terms appearing in the data columns.  This does not fit so nicely in the above table, where each attribute is a single row, so if data uses factors (instead of non -
                                                                                                                                                                                                                                          enumerated strings), these definitions must be provided in a separate table.  The format expected of this table has three columns:`attributeName` (as before), `code`, and `definition`.  Note that `attributeName` is simply repeated for all codes belonging to a common attribute.

In this case we have three attributes that are factors,  To make the code below more readable (aligning code and definitions side by side), we define these first as named character vectors, and convert that to a `data.frame`. (
  The `dplyr::frame_data` function also permits this more readable way to define data.frames inline
).

```{
  r
}

tip.na.rastenie <-
  c(
    rice   = "oriz",
    maize    = "pcenka",
    soybean  = "soja",
    sorghum = "sorgo"
    
  )

vegetaciski.indeks <-
  c(
    LAI  = "Leaf area index",
    GAI      = "Green area index",
    FCOVER  = "Fractional of vegetation cover",
    CI = "Clumping index",
    YAI    = "Yellow area index"
    
  )

## Write these into the data.frame format
factors <-
  rbind(
    data.frame(
      attributeName = "tip na rastenie",
      code = names(tip.na.rastenie),
      definition = unname(tip.na.rastenie)
    ),
    data.frame(
      attributeName = "vegetaciski.indeks",
      code = names(vegetaciski.indeks),
      definition = unname(vegetaciski.indeks)
    )
  )
```

With these two data frames in place, we are ready to create our `attributeList` element:```{
  r
}
attributeList <-
  set_attributes(
    attributes,
    factors,
    col_classes = c(
      "character",
      "Date",
      "Date",
      "Date",
      "factor",
      "factor",
      "factor",
      "numeric"
    )
  )
```


## Data file format

The documentation of a `dataTable` also requires a description of the file format itself.  From where can the data file be downloaded ?  Is it in CSV format, or TSV (tab -
                                                                                                                                                                        separated), or some other format ? Are there header lines that should be skipped ? This information documents the physical file itself, and is provided using the `physical` child element to the `dataTable`.  To assist in documenting common file types such as CSV files, the `EML` R package provides the function `set_physical`, which takes as arguments many of these common options.  By default these options are already set to document a standard `csv` formatted object, so we do not need to specify delimiters and so forth if our file conforms to that.  We simply provide the name of the file, which is used as the `objectName`.  (
                                                                                                                                                                          See examples for `set_physical()` for reading other common variations,
                                                                                                                                                                          analogous to the options covered in R's `read.table()` function.)

```{r}
physical <- set_physical("201032.csv")
```

## Assembling the `dataTable`

Once we have defined the `attributeList` and `physical` file, we can now assemble the `dataTable` element itself. Unlike the old `EML` R package, in `EML` version 2.0 there is no need to call `new()` to create elements.  Everything is just a list.  Template lists for a given class can be viewed with the `emld::template()` function.

```{r}
dataTable <- list(
                 ime = "201032.csv",
                 opis = "Vegetaciski indeksi vo polinjata vo severoistocna Kina",

                 lista_na_atributi = c("character", "Date", "Date", "Date", "factor", "factor", "factor", "numeric"))
                                                                                                                                                                                                                   ```")
```


## Coverage metadata

One of the most common and useful types of metadata is coverage
information, specifying the temporal, taxonomic, and geographic coverage
of the data.  This kind of metadata is frequently indexed by data
repositories, allowing users to search for all data about a specific
region, time, or species.  In EML, these descriptions can take many forms,
allowing for detailed descriptions as well as more general terms when such
precision is not possible (such as geological epoch instead of date range,
or higher taxonomic rank information in place of species definitions.)

Most common specifications can
be made using the more convenient but less flexible `set_coverage()`
function in EML.  This function takes a date range or list of specific
dates, a list of scientific names, a geographic description and bounding
boxes, as shown here:


```{r}
pokrienost <- "Median Latitude: 47.530000 * Median Longitude: 130.165000 * South-bound Latitude: 47.410000 * West-bound Longitude: 126.820000 * North-bound Latitude: 47.650000 * East-bound Longitude: 133.510000"


pokrienost1<-
  set_coverage(begin = '2020 - 06 - 01', end = '2021 - 12 - 31',
               sci_names = "",
               geographicDescription = pokrienost,
               west = 126.82, east = 133.89,
               north = 47.38, south = 47.410,
               altitudeMin = 47.53, altitudeMaximum = 148.89,
               altitudeUnits = "DMS")
```



## Creating methods

Careful documentation of the methods involved in the experimental design, measurement and collection of data are a key part
of metadata.  Though frequently documented in scientific papers, such method sections may be too brief or incomplete, and may become more readily disconnected from the data file itself.  Such documentation is usually written using word-processing software such as MS Word, LaTeX or markdown.  Users with `pandoc` installed (which ships as part of RStudio) can install the `rmarkdown` package to take advantage of its automatic conversion into the DocBook XML format used by EML.  Here we open a MS Word file with the methods and read this into our methods element using the helper function `set_methods()`.  While not used in this example, note that the `set_methods()` function also includes many optional arguments for documenting additional information about sampling, or relevant citations.

```{r eval=has_pandoc}
methods_file <- system.file("examples/hf205-methods.docx", package = "EML")
methods <- set_methods(methods_file)
```

```{r include=FALSE, eval=!has_pandoc}
## placeholder if pandoc is not installed
methods <- NULL
```


## Creating parties

Individuals and organizations appear in many capacities in an EML document.  Meanwhile, R already has a native object class, 
                              `person` for describing individuals, which it uses in citations and package descriptions, among other things.
        We can usenative R function `person()` to create an R `person` object.  Often it is more convenient to use R's coercion function,
             ````{r}                                                                                                                                                             
                R_person <-                                                                                                                                                                                                                                                                                                                  ```
                person(
                  "Fang",
                  "Holiang",
                  ,
                  "fanghl@lreis.ac.cn",
                  "cre",
                  c(ORCID = "0500-0003-4151-6081")
                )
                Fang <-
                  as_emld(R_person)      
                
                 ````                                                                                                                                                         

```{r}
    adresa  <- list(
                  adresa_ = "546 Zhongyang Avenue, st. Yonghui",
                  grad = "Harbin",
                  admistrativnazona = "MA",
                  poshtenskikod = "510000",
                  drzhava = "CHINA")
```


```{r}
  izdavac <- list(
                 Ime_na_organizacijata = "National Natural Science Foundation of CHINA",
                 adresa = adresa)
```


```{r}
   kontakt <-
  list(
     ime= "Fang Hongliang",
    mail_adresa = "fanghl@lreis.ac.cn",
    address = adresa,
    organizationName = "NNSFC",
    phone = "41-171333")

```


## Creating a `keywordSet`

Constructing the `keywordSet` is just a list of lists.  Note that everything is a list.

```{r}
Klucni_zborovi <- list(
    list(
        naslov = "indeksi",
        Kluchen_zbor = list("LAI",
                    "YAI",
                    "GAI",
                    "FCOVER")
        ),
    list(
        naslov = "Sateliti",
        Kluchen_zbor =  list("Landsat 7+", "NDVI", "GIS")
        ),
    list(
        naslov  = "default",
        Kluchen_zbor = list("China crops", "clumping index", "Pagnea", "LTER")
        ))
```


Lastly, some of the elements needed for `eml` object can simply be given as text strings.

```{r}
Datum_na_objava <- "2021"

Naslov <- "Vegetation structural field measurement data for Northeastern China Crops (NECC). PANGAEA"

abstract <- "Field measured structural variables, particularly continuous in-situ data are pivotal for mechanism study and
remote sensing validation. Multiple continuous field measurement campains were conducted in northeastern China crop fields:
Honghe (2012, 2013, 2019) and Hailun (2016) . The Honghe site (47.65?N, 133.51?E) is covered with large homogeneous paddy rice
and the Hailun site (47.41?N, 126.82?E) is planted with maize, sorghum, and soybean. Continuous measurements were made throughout
almost the entire growing season, ranging from day of year (DOY) 160 to 280. For each site, five plots were selected. Typically,
four elemental sampling units (approximately 15 m*15 m) were sampled for each plot to reduce random sampling error. Destructive
sampling, Digital hemispheric photography (DHP), LAI-2200 canopy analyzer, and AccuPAR meausrements were carried out in the field
measurement simultaneously. The green leaf area index (GAI), yellow leaf area index (YAI), and plant area index (PAI, including
the area of leaf, stem, and ear) were measured with destructive sampling. The effective leaf area index (LAIeff), leaf area index
(LAI), fractional of vegetation cover (FCOVER), and clumping index (CI) were derived from DHP and LAI-2200. Since DHP and LAI-2200
cannot separate the stem from leaf, the LAI obtained from DHP and LAI-2200 can be regarded as the PAI.
The fraction of absorbed photosynthetically active radiation (FAPAR) was measured with AccuPAR using the four flux method. "

Avtorski_prava <- "This dataset is released to the public and may be freely
  downloaded. Always quote citation above when using data! You can download the citation in several formats below."
```


Many of these text fields can instead be read in from an external file that has richer formatting, such as we did with
the `set_methods()` step.  Any text field containing a slot named `section` can import text data from a MS Word `.docx` file, markdown file, or other file format recognized by [Pandoc](http://pandoc.org) into that element.  For instance, here we import the same paragraph of text shown above for `abstract` from an external file (this time, a markdown-formatted file) instead:

```{r eval=has_pandoc}
abstract_file <-  system.file("examples/hf205-abstract.md", package = "EML")
abstract <- set_TextType(abstract_file)
```


We are now ready to add each of these elements we have created so far into our `dataset` element, like so:


```{r}
dataset <- list(
               naslov =Naslov ,
               creator = kontakt,
               datum_na_objava = Datum_na_objava,
               avtorski_prava = Avtorski_prava,
                abstract = abstract,
               kluchni_zborovi = Klucni_zborovi,
               pokrienost = pokrienost,
               konttakt = kontakt,
               dataTable = dataTable)
```





With the `dataset` in place, we are ready to declare our root `eml` element.  In addition to our `dataset` element we have already built, all we need is a packageId code and the system on which it is based.  Here we have generated a unique id using the standard `uuid` algorithm, which is available in the R package `uuid`.

```{r}
eml <- list(
           packageId = uuid::UUIDgenerate(),
           system = "uuid", # type of identifier
           dataset = dataset)

```



With our `eml` object fully constructed in R, we can now check that it is valid, conforming to all criteria set forth in the EML Schema.  This will ensure that other researchers and other software can readily parse and understand the contents of our metadata file:


```{r}
write_eml(eml, "eml.xml")
```


```{r}
eml_validate("eml.xml")
```


The validator returns a status `0` to indicate success.  Otherwise, the first error message encountered will be displayed. The most common reason for an error is probably the omission of a required metadata field.


To take the greatest advantage of EML, we should consider depositing our file in a [Metacat](https://knb.ecoinformatics.org/knb/docs/intro.html)-enabled repository, which we discuss in the next vignette on using EML with data repositories.

```{r include=FALSE}
unlink("eml.xml")
```
