#' @name oakwoods
#' @title Oak Woodlands in the Willamette Valley, Oregon, USA
#' @aliases oakwoods
#' @docType data
#' @description
#' Vascular plants in oak forests of the Willamette Valley from the PhD
#'     dissertation of John F. Thilenius at Oregon State University.
#'
#' @format A list of 5 data.frames:
#'
#' - \code{spe} species abundance matrix: 47 observations of 103 vascular
#'     plant species. Abundances were relativized by species maximum.
#'
#' - \code{env} environmental matrix: 47 observations of 30 environmental
#'     variables. Environmental variables, described in detail below, include
#'     topographic, geographic and soils variables, and indicators of stand
#'     history. We also provide some community summary variables, including
#'     species richness, groups derived from cluster analysis, and community
#'     types as originally designated by Thilenius.
#'
#' - \code{tra} traits matrix: 189 vascular plant species by 6 traits. The
#'     traits are simply growth forms and are scored as binary 0/1 (no/yes).
#'
#' - \code{xy} spatial matrix: 47 observations of 2 spatial coordinates.
#'
#' - \code{raw} raw species abundances: 47 observations of 103 vascular plant
#'     species. The raw species abundances are before any modifications. The
#'     values are basal areas (ft^2^/acre) for trees and percentage cover for
#'     lower strata, based on 60, 0.2 m^2^ quadrats/stand. “Trace” was
#'     converted to 0.5\%. A check on the field data sheet was converted
#'     to 0.2\%. Be careful! Any use of these raw data must recognize that the
#'     columns representing the tree stratum differ in units from the lower
#'     strata; hence, use of a relativized matrix in \code{spe}.
#'
#' @details
#'
#' This documentation is nearly verbatim from PC-ORD (McCune and Mefford 2017).
#'
#' @section Overview:
#'
#' In 1961 and 1962 John F. Thilenius sampled vascular plants in oak forests in
#' the Willamette Valley for his Ph.D. at Oregon State University
#' (Thilenius 1963, 1968). The data came from a fairly narrow range of habitats
#' – all of the stands were closed forests dominated by \emph{Quercus garryana}.
#' This resulted in a data set with fairly low beta diversity. The environmental
#' differences among the sites are rather modest. Much of the variation in
#' species composition presumably is derived from the particular histories of
#' each stand, such as episodes of grazing, logging, and fire. Of course we have
#' limited information on those histories, so you will see that much of the
#' variation in the plant communities is not readily explained by the measured
#' environmental and historical variables. Nevertheless a definite environmental
#' gradient emerges from the analysis.
#'
#' The abstract from Thilenius (1968) is reproduced below:
#'
#' “\emph{Quercus garryana} forests, prominent at low elevations throughout the
#' Willamette Valley, Oregon, have developed from oak savanna subsequent to
#' settlement of the valley in the mid-nineteenth century. Interruption of the
#' ground fires that were common in the pre-settlement environment probably
#' caused the change. The understory of the oak forest is dominated by shrubs,
#' and well-defined strata are present. Four plant communities occur: (1)
#' \emph{Quercus garryana/Corylus cornuta var. californica/Polystichum munitum}
#' (most mesic); (2) \emph{Quercus garryana/Prunus avium/Symphoricarpos albus};
#' (3) \emph{Quercus garryana/Amelanchier alnifolia}; (4) \emph{Quercus
#' garryana/Rhus diversiloba} (most xeric). All are in seral condition because
#' of their relatively recent development and because they have been disturbed
#' throughout their existence by man`s activities. The soils supporting the oak
#' forest are generally deep and well drained and have developed profiles with
#' illuvial horizons and acidic reaction. They are derived from sedimentary and
#' basic igneous rocks and old valley-filling alluvium. Seven established soil
#' series are present: Steiwer, Carlton, Peavine, Nekia, Dixonville, Olympic,
#' and Amity. The Steiwer series and its catenary associate, Carlton, are the
#' most common soils.”
#'
#' Thilenius’ goals were to describe “the floristic composition, stand
#' structure, physical environment, and successional status of plant
#' communities where \emph{Quercus garryana} is the major component of the overstory.”
#' Although quantitative data were carefully recorded, Thilenius had few
#' possibilities for multivariate analysis. His primary analyses were first
#' arranging his data “according to similarities in species composition,
#' importance ranks, and environmental attributes.” He then tabulated
#' averages for species and environmental variables within the four groups.
#' Here is an interesting challenge for modern community analysts: what
#' can you add to his account (Thilenius 1968) based on a more sophisticated
#' quantitative analysis of the data? I mentioned above that a single strong
#' environmental gradient emerges from the analysis, but this is only hinted
#' in Thilenius’ abstract. What is that gradient?
#'
#' After a listing of the files and variables contained in the files, three
#' example procedures are given. The first demonstrates modification of the raw
#' data into a form suitable for analysis. The second is an ordination with
#' nonmetric multidimensional scaling. The third compares groups of sample
#' units, as defined by landform.
#'
#' @section Methods from Thilenius (1968):
#'
#' “Investigations were confined to closed-canopy stands 4 ha or more in area
#' where \emph{Quercus garryana} was the major component of the overstory. Basal area,
#' frequency, and density of overstory trees were determined on twenty 0.004-ha
#' circular plots spaced at 9-m intervals in four rows parallel to the slope
#' contour. Density was recorded in four classes: saplings (< 10 cm dbh);
#' poles (11-40 cm dbh); mature (41-100 cm dbh) and relict (> 100 cm dbh).
#' The maximum height of trees on each plot was measured with an optical
#' rangefinder.”
#'
#' “Frequency and percentage crown coverage of shrub and herbaceous species
#' were recorded on sixty 0.2 m2 quadrats spaced at 3-m intervals in four
#' rows coincident with the rows of 0.004-ha plots. Very low crown coverage
#' was recorded as trace and arbitrarily assigned a value of 0.5\% for
#' calculation purposes. Above trace, the intervals were 1\% and 5\%. Coverage
#' greater than 5% was estimated to the nearest 10\%.”
#'
#' @section Coding for variables in the second matrix:
#'
#' \emph{Topographic and geographic variables}
#'
#' Elev,m = elevation above sea level in meters.\cr
#' LatAppx = approximate latitude, decimal degrees, based on automated
#' conversion of Township/Range/Section, using the program TRS2LL.exe.\cr
#' LongAppx = approximate longitude, decimal degrees, based on automated
#' conversion of Township/Range/Section, using the program TRS2LL.exe.\cr
#' SlopeDeg = slope in degrees (originally recorded in percentages)\cr
#' AspClass = aspect class, 1=SW, 2=S or W, 3=SE or NW, 4=N or E, 5=NE.\cr
#' AspDeg = aspect in degrees E of N\cr
#' PDIR = Potential annual direct incident radiation, MJ/cm2/yr, calculated
#' according to McCune and Keon (2002) Eq. 3.\cr
#' HeatLoad = Heat load index, calculated according to McCune and Keon (2002)\cr
#' Landform: 1=valley bottom, 2=draw or slope of draw, 3=slope, 4=ridge\cr
#' TopoClas = Topographic position class: adapted from scales used by
#' Whittaker & Kessell (Kessell 1979)\cr
#'
#' \emph{Soil variables}
#'
#' Drainage: 1=poor, 2=moderate, 3=good, 4=well\cr
#' Soil series: 1=Steiwer, 2=Peavine, 3=Dixonville, 4=Nekia, 5=Carlton,
#' 6=Olympia, 7=Amity\cr
#' SoilGrp: 1=sedimentary, 2=basic igneous, 3=alluvial\cr
#' A-horiz = thickness of A horizon, cm\cr
#' B1-horiz = thickness of B1 horizon, cm\cr
#' B2-horiz = thickness of B2 horizon, cm\cr
#' B3-horiz = thickness of B3 horizon, cm (if profile truncated, e.g.
#'                                         “44+ inches”, add 20 inches)\cr
#' B-horiz = sum of B1+B2+B3, cm\cr
#'
#' \emph{Indicators of stand history}
#'
#' GrazCurr = signs of current grazing recorded on field data sheet
#' (0=no,1=yes)\cr
#' GrazCurrC = same as above but provided as text-based categorical variable
#' (ungrazed, grazed)\cr
#' GrazPast = signs of past grazing recorded on field data sheet
#' (0=no, 1=yes, must be 1 if GrazCurr=1)\cr
#' GrazPastC = same as above but provided as text-based categorical variable
#' (ungrazedpast, grazedpast)\cr
#' NotLogged = NPL recorded under “Influences” on data sheet. I guessed this
#' means “no past logging”, i.e. no signs of past logging
#'  (0=logged, 1=not logged)\cr
#' NotLoggedC = same as above but provided as text-based categorical
#' variable (logged, notlogged)\cr
#' Que>60cm = number of \emph{Quercus garryana} recorded in the 60 cm (24 inch)
#' size class and larger (no stands had large Pseudotsuga; one stand (Stand05)
#' had a large Acer macrophyllum and one stand (Stand07) had two large Arbutus
#' menziesii).\cr
#' LogQ>60 = log of (x+1) where x is the number of \emph{Quercus garryana} recorded
#' in the 60 cm (24 inch) size class and larger (i.e. x = “LogQ>60”).\cr
#' TreeHtM = maximum height of \emph{Quercus garryana} in meters.\cr
#'
#' \emph{Community summary variables derived from the species matrix}
#'
#' SppRich = species richness, calculated from OakRaw.wk1, counting each
#' species x layer combination as a separate species.\cr
#' ThilType = vegetation types from Thilenius (1968)\cr
#'  - 1 = Quercus/Corylus/Polystichum\cr
#'  - 2 = Quercus/Prunus/Symphoricarpos\cr
#'  - 3 = Quercus/Amelanchier/Symphoricarpos\cr
#'  - 4 = Quercus/Rhus\cr
#' FlxB-.25 = community types defined at the 4-group level from hierarchical
#' cluster analysis, Flexible beta method, Sørensen distance, beta= -0.25.\cr
#'
#' @section List of species codes:
#'
#' Note: because woody species may occur in more than one stratum, a suffix
#' (-s, -t) is used to indicate a given species in the shrub or tree stratum.
#'
#' Abgr‑s Abies grandis SHRUB
#' Abgr-t Abies grandis
#' Acar  Actea arguta
#' Acgld Acer glabrum var. douglasii
#' Acma‑s Acer macrophyllum shrub
#' Acma-t Acer macrophyllum
#' Acmi  Achillea millefolium
#' Adbi  Adenocaulon bicolor
#' Agha  Agrostis hallii
#' Agre  Agropyron repens
#' AGRO  Agrostis sp?
#' Agse  Agrostis semiverticullata (subsecundum)
#' Agte  Agrostis tenuis
#' Aica  Aira caryophyllea
#' ALL  Allium sp.
#' Alpr  Alopecurus pratensis
#' Amal‑s Amelanchier alnifolia shrub
#' Amal-t Amelanchier alnifolia
#' Apan  Apocynum androsaemifolium
#' Aqfo  Aquilegia formosa
#' Arel  Arrhenatherum elatius
#' Arme‑s Arbutus menziesii SHRUB
#' Arme-t Arbutus menziesii
#' Avfa  Avena fatua
#' Beaq  Berberis aquifolium
#' Brpu  Brodiaea pulchella
#' Brco  Bromus commutatus
#' Brla  Bromus laevipes
#' Brri  Bromus rigidus
#' Brse  Bromus secalinus
#' Brst  Bromus sterilis
#' Brvu  Bromus vulgeris
#' Caqu  Camassia quamash
#' CAR  Carex sp.
#' Cato  Calochortus tolmiei
#' Cear  Cerastium arenses
#' Ceum  Centaurium umbellatum
#' Ceve  Ceanothus velutinus
#' Cipa  Circaea pacifica
#' Civu  Cirsium vulgare
#' Coco‑s Corylus cornuta shrub
#' Coco-t Corylus cornuta
#' Cogr  Collomia grandiflora
#' Conu‑S Cornus nuttallii SHRUB
#' Conu-t Cornus nuttallii
#' CORY  Corylus sp.
#' Cost  Corallorhiza striata
#' Crca  Crepis capillaris
#' Crdo‑t Crataegus douglasii
#' Crdo‑s Crataegus douglasii
#' Crox  Crataegus oxyacantha
#' Cyec  Cynosurus echinatus
#' Cyfo  Cystopteris fragilis
#' Cygr  Cynoglossum grande
#' Daca  Danthonia californica
#' Dacar Daucus carota
#' Dagl  Dactylus glomerata
#' Deel  Deschampsia elongata
#' Diar  Dianthus armeria
#' Doel  Downingia elegans
#' Drar  Drysopterus arguta
#' Drar  Dryopteris arguta
#' Elgl  Elymus glaucus
#' Erla  Eriophyllum lanatum
#' Erog  Erythronium oregonum
#' Eucr  Euphorbia crenulata
#' Feca  Festuca californica
#' Fede  Festuca dertonenses
#' Feel  Festuca elatior var. arendmaceae
#' Feme  Festuca megalura
#' Feoc  Festuca occidentalis
#' Feru  Festuca rubra
#' Frbr  Fragaria bracteata (vesca)
#' Frcu  Fragaria cuneifolia
#' Frla‑s Fraxinus latifolia shrub
#' Frla-t Fraxinus latifolia
#' Frvi  Fragaria virginiana
#' GAL  Galium sp.
#' Gema  Geum macrophyllum
#' Geog  Geranium oreganum (incisum)
#' Gepu  Geranium pusillum
#' Haob  Habenaria orbiculata
#' Haun  Habenaria unalacensis
#' Hehe  Hedera helix
#' Hemi  Heuchera micrantha
#' Hodi  Holodiscus discolor
#' Hola  Holcus lanatus
#' Hyoc  Hydrophyllum occidentale
#' Hype  Hypericum perforatum
#' Hyra  Hypochaeris radicata
#' Irte  Iris tenax
#' JUNC  Juncus sp.
#' Kocr  Koeleria cristata
#' Laco  Lapsana comunis
#' Lapo  Lathyrus polyphyllus
#' Lasa  Lathyrus sativus (Pisum sativum)
#' Liap  Ligusticum apiifolium
#' Libu  Lithophragma bulbifera
#' Lico  Lilium columbianum
#' Lide-t Libocedrus deccurens
#' Lide‑s Libocedrus deccurens
#' LILI  Lilium sp.
#' Loci  Lonicera ciliosa
#' Lope  Lolium perenne
#' LOT  Lotus sp.
#' Lotr  Lomatium triternatum
#' Lumu  Luzula multiflora
#' Maex  Madia exigua
#' MAL  Malvaceae sp.
#' Maor  Marah oreganus
#' Mebu  Melica bulbosa
#' Mila  Microseris laciniata
#' Mope  Montia perfoliata
#' Mosi  Montia sibirica
#' Nepa  Nemophylla parviflora
#' ONGR  Onagraceae sp.
#' Osce‑t Osmaronia cerasiformis tree
#' Osce-s Osmaronia cerasiformis
#' Osch  Osmorhiza chilensis
#' Osnu  Osmorhiza nuda (chilensis)
#' Phca  Physocarpus capitatus
#' Phle  Philadelphus lewisii
#' Phpr  Phleum pratense
#' Phvi  Phoradendron villosum
#' Pipo‑s Pinus ponderosa
#' Pipo  Pinus ponderosa
#' Plla  Plantago lanceolata
#' Poco  Poa compressa
#' Pogl  Potentilla glandulosa
#' Pogr  Potentilla gracilus
#' Pogr  Potentilla gracilis
#' Pomu  Polystichum munitum
#' Popr  Poa pratensis
#' Povu  Polypodium vulgare
#' Prav‑s Prunus avium shrub
#' Prav-t Prunus avium
#' Prde-t Prunus virginiana var. demissa
#' Prde‑s Prunus virginiana var. demissa shrub
#' Prvu  Prunella vulgeris
#' Psme‑s Pseudotsuga menziesii shrub
#' Psme-t Pseudotsuga menziesii
#' Ptan  Pterospora andromedia
#' Ptaq  Pteridium aquilinum var. lanuginosum
#' Pyco‑s Pyrus communis shrub
#' Pyco  Pyrus communis
#' Pyfu‑s Pyrus fusca SHRUB
#' Pyfu-t Pyrus fusca
#' Quga-s Quercus garryana shrub
#' Quga  Quercus garryana
#' Raoc  Ranunculus occidentalis
#' Rhdi  Rhus diversiloba
#' Rhpu‑s Rhamnus purshiana shrub
#' Rhpu  Rhamnus purshiana
#' Risa  Ribes sanguinius
#' Rodu  Rosa???
#' Roeg  Rosa eglanteria
#' Rogy  Rosa gymnocarpa
#' Ronu  Rosa nutkana
#' Ropi  Rosa pisocarpa
#' Ropi  Rosa pisocarpa
#' Ruac  Rumex acetosella
#' Rula  Rubus laciniatus
#' Rule  Rubus leucodermus
#' Rupa  Rubus parvifloris
#' Rupr  Rubus procerus
#' Ruur  Rubus ursinus
#' S‑2  Carex sp2.
#' S‑1  Carex sp1.
#' Sacr  Sanicula crassicaulis
#' Sado  Satureja douglasii
#' Sagr  Sanicula graveolens
#' Seja  Senecio jacobaea
#' Siho  Silene hookeri
#' Smra  Smilacina racemosa
#' Smse  Smilacina sessilifolia
#' Syal  Symphoricarpus albus
#' Taas  Taeniatherum asperum
#' Taof  Taraxacum officinale
#' Tegr  Tellima grandiflora
#' Thoc  Thalictrum occidentale
#' Toar  Torilis arvensis
#' Trca  Trisetum canescens
#' TRIF  Trifolium sp
#' Trla  Trientalis latifolia
#' Trov  Trillium ovatum
#' Trpr  Trifolium procumbens
#' V1    Vicia sp.
#' Valo  Valerianella locusta
#' Viam  Vicia americana
#' Viel  Viburnum ellipticum
#' Vinu  Viola nuttallii
#' VIOL  Viola sp
#' Zice  Zygadenus venosus
#'
#' @source Bruce McCune collected this dataset for PC-ORD (McCune and Mefford
#'     2017). The original raw data cards from Thilenius study in 1963
#'     (data collected in 1961 and 1962) were obtained from John Thilenius
#'     via Bob Frenkel. Thanks to John Thilenius for granting permission to
#'     distribute his data. Bill Daly did the initial data entry. Bibit Traut
#'     added more variables and resolved numerous nomenclatural questions
#'     regarding the species codes used by Thilenius.
#'
#' @references
#' Kessell, S. R. 1979. Gradient Modeling: Resource and Fire Management.
#'     Springer-Verlag, New York. 432 pp.
#'
#' McCune, B. and D. Keon. 2002. Equations for potential annual direct
#'     incident radiation and heat load. Journal of Vegetation Science
#'     13:603-606.
#'
#' McCune, B., and M. J. Mefford. 2017. PC-ORD. Multivariate Analysis
#'     of Ecological Data. Version 7. MjM Software Design, Gleneden
#'     Beach, OR.
#'
#' Thilenius, J. F. 1963. Synecology of the white-oak (\emph{Quercus garryana}
#'     Douglas) woodlands of the Willamette Valley, Oregon. PhD Dissertation.
#'     Oregon State University, Department of Botany and Plant Pathology,
#'     Corvallis. 151 pages.
#'
#' Thilenius, J. F. 1968. The \emph{Quercus garryana} forests of the Willamette
#'     Valley, Oregon. Ecology 49:1124-1133.
#'
#' @examples
#' # split into two data.frames
#' data(oakwoods)
#' spe <- oakwoods$spe
#' env <- oakwoods$env
#' tra <- oakwoods$tra
#' raw <- oakwoods$raw
#'
#' @keywords datasets
"oakwoods"
