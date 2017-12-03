               The Oxford Lake-Level Data Bank: Readme File 
-----------------------------------------------------------------------
               World Data Center A- Paleoclimatology
-----------------------------------------------------------------------
NOTE: PLEASE CITE ORIGINAL REFERENCE WHEN USING THIS DATA!!!!!

CONTRIBUTORS:  COHMAP members
IGBP PAGES/WDCA Data Contribution Series #:  94-028
NAME OF DATA SET:  The Oxford Lake-Level Data Bank
LAST UPDATE:  10/94 ( Original Receipt by WDC-A Paleo)
GEOGRAPHIC REGION: Global
PERIOD OF RECORD:  0-20,000 YBP

LIST OF FILES:
The following files contain the following information:
1.  lakeres.txt         Lake Stand and Dating Info, with descriptive headers
2.  lakedata.txt        Lake Stand and Dating Info, without headers
3.  lakelevlindex.txt   Site information for all lakes       
4.  numrefs.txt         publication reference list
5.  pubindex.txt        list of BMN and corresponding publication number
All files can be downloaded from this FTP folder:
ftp://ftp.ncdc.noaa.gov/pub/data/paleo/paleolimnology/lakelevels/oxford/ 

ORIGINAL REFERENCE:
Street-Perrott, F.A., D.S. Marchand, N. Roberts, and S.P Harisson, 1989.
Global Lake-Level Variations from 18,000 to 0 Years Ago: A Paleoclimatic 
Analysis.   U.S. Department of Energy Technical Report 46, Washington, 
D.C. 20545.   Distributed by National Technical Information Service, 
Springfield, VA 22161.

SUGGESTED DATA CITATION:
COHMAP Members, 1994, Oxford Lake Levels Database.  IGBP PAGES/World Data Center-A 
for Paleoclimatology Data Contribution Series # 94-028.  NOAA/NGDC Paleoclimatology Program,
Boulder CO, USA.


DESCRIPTION:
APPENDIX A. CRITERIA USED TO DETERMINE LAKE STATUS AND
            DATING CONTROL.


Lake Status

       Lake status was originally determined at 1000-year
intervals between 30 ka BP and the present day (0 ka BP =
1950 AD), although only the data sets for 3000-year
intervals since 18 ka BP are presented here. The sampling
scheme used in this report has the effect of suppressing
shorter-term variations, which tend to reflect regional
climatic and hydrological perturbations, and emphasizing
longer-term global variations.

      A standardization procedure was applied in order to
render all basins comparable, regardless of size. The total
range of fluctuation experienced by each basin during the
past 30,000 years was divided into three lake-status
categories with a similar overall frequency of occurrence in
the data set (Street and Grove, 1976 ,1979):

LOW           0-15% of the total altitudinal range of
              fluctuation, including dry lakes
INTERMEDIATE  15-70% of the total range
HIGH          70-100% of the total range of fluctuation,
              including overflowing lakes.

For basins where the absolute range of lake levels could not
be determined, the relative lake level was coded in the
following way: when the lake was at or near its deepest or
freshest, it was coded as HIGH and when at or near its
lowest or most saline, as LOW. Periods for which there was
unambiguous evidence that the lake fell between these states
were assigned to the INTERMEDIATE class.

For many of the basins in the data bank, a continuous record
of water level has been published or made available to us.
In such cases, it was possible to determine the trend in
water level at 1000-year intervals as RISING, STABLE or
FALLING. If no trend information was available for a
particular time period, trend was classified as UNCODABLE.


2. Dating Control

     Dating control is a relative ranking of the quality of
the chronology used to select the following time levels: 18
ka BP; 15 ka BP 12 ka BP 9 ka BP 6 ka BP and 3 ka BP.
Two different schemes were applied, depending on whether the
dates were derived from a continuous sequence such as a core
or from discontinuous samples such as shoreline tufas; the
dating control was assessed for each time period on a scale
of decreasing quality ranging from one to seven.

     The coding for continuous sequences was based on the
following criteria (extended from Webb, 1985):

1. Bracketting dates within 2000 years of the selected
   date. The top of a continuous core can be assigned a
   date of 0 ka BP.
2. Bracketting dates, one within 2000 years and the
   other within 4000 years of the selected date.
3. Bracketting dates within 4000 years of the selected
   date.
4. Bracketting dates, one within 4000 years and the
   other within 6000 years of the selected date.
5. Bracketting dates within 6000 years of the selected
   date.
6. Bracketting dates, one within 6000 years and the
   other within 8000 years OR one within 4000 years
   and the other within 10,000 years of the selected
   date.
7. Undated at selected date: either no dates in the
   core, no bracketing dates, no top to core and no
   date within 8000 years of the selected date, or
   bracketing dates more than 14,000 years apart.
0. Status uncodable for selected date regardless of
   availability of radiocarbon dates.

     Additionally, a ranking of 1 was allotted to a
continuous sequence if there was a single date within 2o or
250 years of the selected level.


     The coding for discontinuous sequences was based on the
following criteria (extended from Webb, 1985):

1. At least one radiocarbon date within 250 years of
   the selected level.
2. At least one radiocarbon date within 500 years of
   the selected level.
3. At least one radiocarbon date within 750 years of
   the selected level.
4. At least one radiocarbon date within 1000 years of
   the selected level.
5. At least one radiocarbon date within 1250 years of
   the selected level.
6. At least one radiocarbon date within 2000 years of
   the selected level.
7. Radiocarbon date(s) more than 2000 years from the
   selected level.
0. Status uncodable for selected level regardless of
   availability of radiocarbon dates.


APPENDIX B. DESCRIPTION AND FORMAT OF THE OXFORD LAKE-LEVEL
            DATA BANK


The Oxford Lake-Level Data Bank comprises records of lake
status, a measure of relative water depth, for lake basins
which have been closed for part, or all, of their Late
Quaternary history. The data base is arranged regionally in
the following order:
            Africa;
            South and Southwest Asia (including Afghanistan
                                      and India);
            China;
            Soviet Union;
            North America (U.S.A. and Canada, in that
                           order);
            Mexico;
            South and Central America;
            Australasia (Australia, New Zealand and Papua
                         New Guinea, in that order);
            Europe and Greenland.

Within each regional block the order of the basins is
alphabetical. For North America, basins are listed
alphabetically by state or province.

The full data bank is stored, in slightly different formats,
in the files LAKEDATA.DAT and LAKERES.DAT. These files
contain the lake status and trend codes, with information
about supporting radiocarbon dates for each 1000-year time
slice for each basin. Each basin is uniquely identified by a
COHMAP Basin number, called the Brown Master Number (BMN)
(see Appendix C). This number permits quick reference to be
made between the full data bank and supporting files
(Appendices C, D and E). LAKERES.DAT contains headings to
help the reader understand the data. However, LAKEDATA.DAT
contains the raw data without descriptive headings.
Therefore, its format is described below.

Format of the file LAKEDATA.DAT

The first line of the data base provides information about
the first lake in the file, Lake Abhe, Ethiopia. The
beginning of the information for each basin is signified by
a line with its name, location and Brown Master Number. The
number of lines required for each basin is variable,
depending on the number of time periods classified and the
number of radiocarbon dates for each time period. However,
the format of lines 1-6 is the same for each basin. The
format for the remainder of each basin entry is flexible,
depending on the number of time periods and the available
dating control.

The following information is included for each basin:

      1st line - columns  1-35  basin name (state or
                                province) and country
                 columns 37-42  latitude as a decimal (+ve
                                values for Northern
                                Hemisphere and -ve values
                                for Southern Hemisphere)
                 columns 46-52  longitude as a decimal (+ve
                                values for Eastern
                                Hemisphere and -ve values
                                for Western Hemisphere)
                 columns 55-58  Brown Master Number - a
                                sequential code identifying
                                each basin by the order in
                                which it was added to the
                                data base;

      2nd-5th lines - a quick guide to source references,
                      including those with radiocarbon
                      dates, along with any important
                      qualifying information;

      6th line - columns  1- 2  time period (1 = present
                                day, 2 = 1 ka BP, 3 = 2 ka
                                BP, etc.)
                 column   4     lake status (1 = high,
                                2 = intermediate, 3 = low,
                                0 = uncodable)
                 column   6     lake trend (1 = rising,
                                2 = stable, 3 = falling,
                                0 = uncodable)
                 columns  10-11 number of radiocarbon dates
                                within the corresponding
                                time period

If there are no radiocarbon dates for a time period, the
following line provides the same information as line 6 for
the preceding time period. If radiocarbon dates are
available there follow two additional lines for each date
with the following structure:

     line A      columns   8-11 radiocarbon date in years
                                BP
                 columns  16-20 +ve standard error of the
                                radiocarbon date
                 columns  26-30 -ve standard error of the
                                the radiocarbon date
     line B      columns   4-5  number of reference listing
                                radiocarbon date (see lines
                                2-5 above)
                 columns   6-15 laboratory number of the
                                radiocarbon date
                 columns 16-132 information about the
                                material dated, its
                                location and validity (see
                                below for list of
                                abbreviations used)

After the two lines of information for each radiocarbon date
for a given time period have been entered, the succeeding
line contains either the information for the next time
period (in the same format as line 6 above), or, if there is
no more information for this basin, the terminator '808080
80' in columns 1-11. The end of the data bank is signified
by the terminator '909090  90' in columns 1-11.

The format of LAKEDATA.DAT is illustrated below.


FORMAT OF THE FILE LAKEDATA.DAT

                                                                                   1
            1         2         3         4         5         6         7          3
   1234567890123456789012345678901234567890123456789012345678901234567890.....56789012


   BASIN, STATE OR PROVINCE, COUNTRIES LATITU   LONGITU  BMN#
   R#> AUTHORS,DATE<R#> AUTHORS,DATE ....                                         ....
   ....                                                                           ....
   ....                                                                           .....
   ....COMMENTS1....
   TP S T #C14
        ESTAG     SE+VE     SE-VE
      R#  LABNUM  MATERIAL DATED AND COMMENTS
         .
         .
         .
   TP S  T #C14
        ESTAG     SE+VE     SE-VE
      R#  LABNUM MATERIAL DATED AND COMMENTS
         .
         .
         .
    .
    .
    .
   808080   80
   BASIN, STATE OR PROVINCE, COUNTRIES LATITU   LONGITU  BMN#
    .
    .
    .
   808080   80
    .
    .
    .
   909090   90
            where  1234... = columns
                   LATITU  = basin latitude in decimal form; positive values for Northern
                             Hemisphere, negative values for Southern Hemisphere; columns 37-42.
                   LONGITU = basin longitude in decimal form; positive values for Eastern
                             Hemisphere, negative values for Western Hemisphere; columns 46-52.
                   BMN#    = Brown University Master Number; columns 55-58
                   R#      = reference number - used to identify sources of radiocarbon dates
                   ....    = continuation of information in same format
                   COMMENTS1 = comments relating specifically to the references; not present for
                               all basins; follows immediately after last reference or at
                               beginning of next line
                   TP      = Time Period; 1000 year periods centered on the millenia; columns 1-2
                             e.g., TP 1 = 0 yr BP (covers period 0 - 499 yr BP),
                                   TP 2 = 1000 yr BP (covers period 500 - 1499 yr BP)
                                    .
                                    .
                                   TP 31 = 30000 yr BP (covers period 29500-30499 yr BP)
                   S       = lake level status; column 4
                                3 = LOW; 2 = INTERMEDIATE; 1 = HIGH; 0 = UNCODABLE
                  T        = lake level trend; column 6
                                3 = FALLING; 2 = STABLE; 1 = RISING; 0 = UNCODABLE
                   #C14    = number of radiocarbon dates falling within the time period under
                             consideration, columns 8-11
                   ESTAG   = radiocarbon age of the sample analyzed; columns 6-10
                   SE+VE   = positive standard error of the radiocarbon age; columns 16-20
                   SE-VE   = negative standard error of the radiocarbon age; columns 26-30
                   LABNUM  = Laboratory analyzing the sample and the assigned sample number;
                             columns 6-15
                   C0MMENTS2 = comments relating specifically to the sample, e.g., clarifying its
                               location or elevation, stating any problems with the date; see
                               'ABBREVIATIONS USED IN THE DATA BASE' for further clarification;
                               along with MATERIAL DATED found in columns 16-132
                   808080   80 = terminator the information for this basin; columns 1-11
                   909090   90 = terminates the data base (also the end of information
                                 for the last basin in the listing); columns 1-11.


Abbreviations used in the Data Bank

 No Data

    0   Numeric columns
  N.A.  Not available
 PCNA   Permanent lab. number unavailable; temporary number
        given in source reference used

 Lake Level
   (Used if individual dates disagree with classification of
   time period or for clarification)

   S1   High (= status 1)
   S2   Intermediate
   S3   Low

 Sediment
   (Describes sediment type)

 SEDS.  Sediments (e.g., CORE SEDS.)
    L   Lacustrine or lake
   LK   Lacustrine or lake
   NL   Nonlacustrine (e.g., for charcoal in soils)
   FL   Fluviolacustrine
    M   Marsh deposits
 LSST   Lake-side archaeological site, indicates lake nearby
 ARCH SITE   Stratigraphically-related archaeological site,
             but not lake-side
  LST   Limestone
    /   Overlying (e.g., where dated deposit overlies beach
        material)

 Materials
   (Describes materials dated}

 MATL. UNCERT.   Material unknown or reference ambiguous
 PROB. -         Reference ambiguous but implies -
 QUERY MATL.     Used where reference unclear or description
                 doubtful
 PLANT MACRO     Plant macrofossils
 DIAT. MARL      Diatomaceous marl
 DIAT.           Diatomite
 CALCAR. CRUST   Calcareous crust (may be pedogenic)
 STROMAT.        Stromatolite or stromatolitic (e.g., STROMAT.
                 LST.)
 LST.            Limestone
 SHELLS          Shells (taxon given if known e.g.,
                 MELANOIDES SHELLS, otherwise type given e.g.,
                 NL SHELLS)
 PROB. SHELL     Probably a date on shell (see note above)
 REWORKED or     Where reworking of dated material known to
   RWKD          have occurred
 PROB. RWKD      Where reworking of dated material suspected
 WXED            Dated material shows signs of weathering,
                 and may be RWKD
 CONCN.          Concentration
 INN.            Inner fraction (e.g., of shells, carbonate
                 nodules) dated
 OUT.            Outer fraction (e.g., of shells, carbonate
                 nodules) dated
 CARB.           Dated on carbonate fraction (CaC03). This
                 often follows MARL, ALGAL LST or TUFA, to
                 clarify fraction dated.
 ARAG.           Aragonite
 AUTH.           Authigenic
 W/              With
 BONE            Bone. Fraction dated (e.g., APATITE or
                 COLLAGEN) usually given if known.
                 Identification (e.g., GIRAFFE  BONE)  usually
                 given if known.
 ORG.FR.         Organic fraction dated (e.g., of CORE MATL.)
 HUM.            Humic
 MATRIX          Matrix of
 CORE MATL.      Core material
 DISS.           Disseminated (usually applies to ORG. FR. or
                 CHARCOAL)
 EVAPTS.         Evaporites

 Validity
    (Assessment of validity of a radiocarbon date)

 QUERY DATE      Suspected typographical or other error in
                 date
 QUERY S.D.      Suspected typographical or other error in
                 standard error
 VD              Validity doubtful, based on source reference
 CS              Contamination suspected, based on source
                 reference
 ATY             Age too young, based on source reference
 ATO             Age too old, based on source reference
 ATO, REJECT     Date definitely invalid and therefore not
                 used in assessment
 POSS. ATY       Assessment based on reinterpretation of
                 source reference or on new data
 POSS. ATO       Assessment based on reinterpretation of
                 source reference or on new data
 STRATIG. INCONSIST.   Date stratigraphically inconsistent
 QUERY DIAGENESIS      Diagenesis suspected, and may be cause
                       of invalid date
 STRATIG. ERR.   Stratigraphic error
 TH-             Corresponding uranium-series date
 AV( )           Date cited is an average of ( ) number of
                 determinations

 Relation to other dates

 DUPL.           Duplicate; lab number of other date usually
                 cited for reference

 Other
    (Information relating to interpretation of data)

 MIN. AGE FOR    Dated material gives minimum age for (e.g.,
                 MIN. AGE FOR BEACH indicates that date
                 derived from an overlying unit or material
                 which must post-date lake, such as packrat
                 midden material from a lake-shore cave)
 ALMOST CERTAINLY INFINITE   Date probably beyond the range
                             of radiocarbon dating, usually
                             associated with VD,CS
 SIGNIF. UNCERT. Significance of a date in relation to lake-
                 level history uncertain. May be applied to
                 dates that are difficult to interpret or
                 archaeological dates where exact relation to
                 shoreline position is unclear.

 Specific comments on individual basins

    COCHISE, ARIZONA
       WWM       Willcox White Marl
       LGC       Lower Green Clay
       IG        Intermediate Gravel
       UGC       Upper Green Clay

     SEARLES, CALIFORNIA
       BM        Bottom Mud
       LS        Lower Salt
       PM        Parting Mud
       US        Upper Salt
       OM        Overburden Mud
       ( )       Indicates inferred correlation (/used here for
                 or)
       Core numbers are listed e.g., L-U-1

     BONNEVILLE, UTAH
       AF        Alpine Formation
       WM        White Marl, Bonneville Formation
       UM        Upper Member, Bonneville Formation
       DF        Draper Formation

     TULARE, CALIFORNIA
       BVL       Buena Vista Lake


APPENDIX C: SITE INFORMATION AND DATING CONTROL FOR THE
            OXFORD LAKE-LEVEL DATA BANK

The file LAKELEVLINDEX.DAT, listed below, provides summary
information about the basins comprising the Oxford Lake
Level Data Bank. They are entered in numerical order, based
on their Brown Master Numbers (BMN). There are two lines of
information for each basin with the following format:

Line   Columns     Content

 1       3- 6     Brown Master Number(BMN)
        10-12     Basin Number
        15-45     Basin Name, Province or State, and Country
                  or Countries
        47-53     Latitude in degrees, minutes and
                  hemisphere
        55-60     Latitude as a decimal (positive values for
                  Northern Hemisphere; negative for Southern
                  Hemisphere)
        63-70     Longitude in degrees, minutes and
                  hemisphere
        73-79     Longitude as a decimal (positive numbers
                  for Eastern Hemisphere; negative for
                  Western Hemisphere)
        84-87     Elevation above sea level in meters

  2     12-14     Country code (see below)
        16-17     Sitetype code (see below)
        21-22     Datatype code(see below)
        26-29     Brown Master Number (BMN)
        33-35     Number of radiocarbon dates
           41     Dating control for 3 ka BP *
           47     Dating control for 6 ka BP *
           53     Dating control for 9 ka BP *
           59     Dating control for 12 ka BP *
           65     Dating control for 15 ka BP *
           71     Dating control for 18 ka BP *
        74-75     Dating method code (see below)

* Please note that categories in line 41 -71 are now different
from those in the original Oxford lake-level Database Readme 
file.  They are now correct according to communication with
Sandy Harrison, Lund, Sweden.  (3/95)

The codes referred to above were devised to include all the
diverse types of information used within COHMAP projects.
Listed below are those codes which apply to this report; for
a full listing of codes used by COHMAP see Webb (1985). For
an explanation of dating control see Appendix A.


Country Codes

 11  Finland               60  Mexico
 14  Greece                61  Namibia
 16  Hungary               62  New Zealand
 19  Italy                 63  Niger
 26  Poland                64  Saudi Arabia
 28  Scotland              65  Sinai
 35  Greenland             66  South Africa
 36  United States         67  Sudan
 37  Canada                68  Syria
 38  Afghanistan           69  Tanzania
 39  Algeria               70  Turkey
 40  Australia             71  Uganda
 41  Bolivia               72  Venezuela
 42  Botswana              73  Soviet Union
 43  Chad                  74  Ethiopia/Kenya
 44  Chile                 75  Ethiopia/Djibouti
 45  China                 76  Jordan/Israel
 46  Colombia              77  Nigeria/Niger
 47  Djibouti              78  Rwanda/Burundi/Zaire
 48  Ecuador               79  Uganda/Tanzania/Kenya
 49  Egypt                 80  Uganda/Zaire
 50  Ethiopia              82  Argentina
 51  Ghana                 93  Tunisia
 52  India                 97  Jamaica
 53  Iran                 100  Morocco
 54  Jordan               121  Guatemala
 55  Kenya                128  Papua New Guinea
 56  Libya                130  Madagascar
 57  Malawi               131  Zambia
 58  Mali                 132  Burundi/Tanzania/Zambia/Zaire
 59  Mauritania           133  Bolivia/Peru


Sitetype Code

 L   Lake


Datatype Code

 LL  Lake Level:    Lake Level Data


Dating Method Codes

  1  14C dates
 10  Pollen Stratigraphic Dates


