

library(sf)
library(jsonlite)
library(httr)
library(foreach)
library(data.table)
library(readxl)

iucn<-as.data.frame(read_excel("C:/Users/God/Downloads/Tableau_Pré-évaluation LR 2022_Diff_contrib_frousseu.xlsx",skip=1))#range="A6:K1116"))
iucn$sp<-gsub("var. |subsp. ","",iucn$"Nom scientifique (sans auteur)")
iucn$nbmailles<-iucn$"Nb mailles de 2x2 km"

# include reviewed_by me 
# https://api.inaturalist.org/v1/docs/#!/Observations/get_observations


# https://www.inaturalist.org/pages/api+recommended+practices
# x<-GET("https://www.inaturalist.org/users/api_token")
# 24 h

api_token<-"eyJhbGciOiJIUzUxMiJ9.eyJ1c2VyX2lkIjoyMTI5MjAxLCJleHAiOjE2NjMzMDgxNDF9.f64aoBPjCP03grM35rpl9MaEPtBfl38fJm8z6OdyLEOplPbt6JMKZjbihG9w9F64EL88sHIIvDXwGrtfdlqKCw"

#st_layers("C:/Users/God/Downloads/carto_maille22_uicn_all_V1.gpkg")
occs<-st_read("C:/Users/God/Downloads/carto_maille22_uicn_all_V1.gpkg")
occs$sp<-gsub("var. |subsp. ","",occs$"nom_sans_auteur")
mailles<-st_read("C:/Users/God/Downloads/maille22.gpkg")
mailles$maille22<-mailles$Nom_Maille2X2
mailles$cell<-1:nrow(mailles)

### fill occs with maille numbers
table(st_is_empty(occs)) # empty geometries
table(is.na(occs$maille22)) # cell without names
occs<-occs[!st_is_empty(occs),]

par(mar=c(0,0,0,0))
plot(st_geometry(mailles))
plot(st_geometry(st_centroid(occs)),add=TRUE)
o<-st_intersects(st_centroid(occs),mailles)
#plot(st_geometry(occs[lengths(o)==0,]),add=FALSE,col="red")
occs$maille22<-mailles$maille22[unlist(o)]

setdiff(occs$maille22,mailles$maille22)
setdiff(mailles$maille22,occs$maille22)
# NA in occs?

api<-"https://api.inaturalist.org/v1/observations?user_id=frousseu&geo=true&verifiable=true&place_id=8834&taxon_id=47126%2C47161%2C52642&hrank=genus&lrank=subspecies&order=desc&order_by=created_at&per_page=200&page=1"
#x<-fromJSON(api)
req<-GET(api,httr::add_headers("Authorization"=api_token))
json<-content(req,as="text")
x<-fromJSON(json)
pages<-ceiling(x$total_results/200)


inatjson<-foreach(i=1:pages,.packages=c("jsonlite")) %do% {
  page<-paste0("page=",i)
  #x<-fromJSON(gsub("page=1",page,api))
  req<-GET(gsub("page=1",page,api),httr::add_headers("Authorization"=api_token))
  json<-content(req,as="text")
  x<-fromJSON(json)
  myid<-sapply(1:nrow(x$results),function(j){
    iders<-x$results[j,]$identifications[[1]]$user$login
    iders[!x$results[j,]$identifications[[1]]$current]<-NA # removes removed ids
    stopifnot(any(!is.na(iders)))
    ids<-x$results[j,]$identifications[[1]]$taxon$name
    k<-length(iders) + 1L - match("frousseu", rev(iders)) # match last frousseu ID
    stopifnot(length(k)==1)
    ifelse(!is.na(k),ids[k],NA)
  })
  inat<-data.frame(
    sp=x$results$taxon$name,
    id=x$results$taxon$id,
    idobs=x$results$id,
    myid=myid,
    location=x$results$location,
    private_location=x$results$private_location,
    positional_accuracy=x$results$positional_accuracy,
    grade=x$results$quality_grade,
    url=paste0("https://www.inaturalist.org/observations/",x$results$id)
  )
  row.names(inat)<-((i-1)*200+1):(((i-1)*200+1)+nrow(inat)-1)
  cat("/r",paste(i,pages,sep=" / "))
  inat
}  
inat<-do.call("rbind",inatjson)
inat$location<-ifelse(!is.na(inat$private_location),inat$private_location,inat$location)
inat$lon<-as.numeric(sapply(strsplit(inat$location,","),"[",2))
inat$lat<-as.numeric(sapply(strsplit(inat$location,","),"[",1))
inat<-st_as_sf(inat,coords=c("lon","lat"),crs=4326)
inat<-st_transform(inat,st_crs(mailles))
o<-st_intersects(inat,mailles)
inat$maille22<-mailles$maille22[unlist(o)]
inat$cell<-mailles$cell[unlist(o)]
inat<-inat[grep(" ",inat$myid),]
inat[which(inat$sp!=inat$myid),c("sp","myid")]
inat<-as.data.table(inat)


### get taxref names from checklistbank.org
url<-"https://api.checklistbank.org/dataset/139831/nameusage/https%3A%2F%2Fwww.inaturalist.org%2Ftaxa%2F"
species<-unique(inat$myid)
species<-species[!species%in%c("Elatostema fagifolium","Strophocaulon unitum")]
l<-lapply(species,function(i){
  print(match(i,species))
  x<-fromJSON(paste0(url,inat$id[match(i,inat$myid)],"/related?datasetKey=2008"))
  if(length(x)>0){
    k<-x$status=="accepted"
    if(any(k)){
      sp<-unique(x$name$scientificName[k])
    }else{
      sp<-unique(x$name$scientificName)
    }
    g<-grep(" × ",sp)
    if(length(g)>0 && length(sp)>1){
      sp<-sp[-g]
    }
  }else{
    sp<-NA
  }
  sp
})
names(l)<-species
species[sapply(l,length)==0]
species[sapply(l,length)==2]

taxrefnames<-data.table(myid=species,taxref=unlist(l,use.names=FALSE))
inat<-merge(inat,taxrefnames,all.x=TRUE)
inat$cbnm<-inat$taxref
inat$cbnm<-gsub("var. |subsp. ","",inat$cbnm)


lapply(unique(inat$myid[is.na(inat$taxref)]),function(i){c(i,"")})

# Osmunda regalis, Phymatosorus scolopendria,Abrodictyum tamarisciforme
lookup<-list(c("Benthamia chlorantha","Benthamia latifolia"), 
             c("Dactyloctenium ctenoides","Dactyloctenium ctenioides"), 
             c("Elatostema fagifolium","Elatostema fagifolium"), 
             c("Strophocaulon unitum","Sphaerostephanos unitus"), 
             c("Korthalsella gaudichaudii","Korthalsella gaudichaudii"),
             c("Microsorum scolopendria","Phymatosorus scolopendria"),
             c("Trichomanes tamarisciforme","Abrodictyum tamarisciforme")
)
lookup<-as.data.frame(do.call("rbind",lookup))
setnames(setDT(lookup),c("old","new"))
inat[lookup,cbnm:=new,on=.(myid=old)]

unique(inat$myid[is.na(inat$cbnm)])
inat[is.na(inat$cbnm),.(myid,sp,taxref,cbnm)]


lookup<-list(c("98817904","Asplenium daucifolium viviparum"), 
             c("128191620","Asplenium daucifolium viviparum"),
             c("100524864","Aristida mauritiana"),
             c("102229784","Aristida mauritiana"),
             c("106815039","Aristida mauritiana"),
             c("109945913","Aristida mauritiana"),
             c("112191362","Aristida mauritiana"),
             c("112192241","Aristida mauritiana")
)
lookup<-as.data.frame(do.call("rbind",lookup))
setnames(setDT(lookup),c("old","new"))
lookup[,old:=as.integer(old)]
inat[lookup,cbnm:=new,on=.(idobs=old)]



miss<-setdiff(iucn$sp,occs$sp)
iucn[!iucn$sp%in%occs$sp,c("sp","nbmailles")]

###
species<-unique(iucn$sp)
loccs<-lapply(species,function(i){
  w<-which(occs$sp==i) 
  if(length(w)>=1){
    old<-sort(unique(occs$maille22[w]))
  }else{
    old<-NA
  }
  w<-which(inat$cbnm==i) 
  if(length(w)>=1){
    mine<-sort(unique(inat$maille22[w]))
  }else{
    mine<-NA
  }
  new<-setdiff(mine,old)
  new<-if(length(new)>0L){new}else{NA}
  nbold<-if(!all(is.na(old))){length(old)}else{0}
  nbnew<-if(!all(is.na(new))){length(new)}else{0}
  listnew<-if(!all(is.na(new))){paste(new,collapse=", ")}else{""}
  listnewid<-if(!all(is.na(new))){paste(mailles$cell[match(new,mailles$maille22)],collapse=", ")}else{""}
  data.frame(species=i,nbold=nbold,nbnew=nbnew,listnew=listnew,listnewid=listnewid)
})
names(loccs)<-species
res<-do.call("rbind",loccs)
row.names(res)<-1:nrow(res)
obsurl<-sapply(seq_along(species),function(i){
  w<-which(inat$cbnm==species[i])
  if(any(w)){
    ids<-inat$idobs[w]
    newids<-strsplit(res$listnewid[which(res$species==species[i])],", ")[[1]]
    ids<-ids[inat$cell[w]%in%newids]
    if(length(ids)>0){
      ids<-paste(ids,collapse=",")
      paste0("https://www.inaturalist.org/observations/?id=",ids,"&place_id=any")
    }else{
      ""
    }
  }else{
    ""
  }
})
res$url<-obsurl
res$empty<-""
write.table(res[,c("species","listnewid","empty","url")][,-1],file="C:/Users/God/Documents/cbnm/iucn.txt",quote=FALSE,row.names=FALSE,sep="\t",col.names=FALSE)


plot(st_geometry(mailles))
plot(st_geometry(st_as_sf(inat,crs=st_crs(mailles))),add=TRUE,col="red")



inatsp<-st_as_sf(inat,crs=st_crs(mailles))
sp<-"Cissus quadrangularis"
par(mar=c(0,0,0,0))
plot(st_geometry(mailles))
plot(st_geometry(occs[occs$nom_sans_auteur==sp,]),col=adjustcolor("darkgreen",0.5),add=TRUE)
plot(st_geometry(inatsp[inatsp$myid==sp,]),col=adjustcolor("red",0.5),cex=2,pch=16,add=TRUE)



#######################################
### OLD STUFF #########################
#######################################


#inat<-inat[inat$grade=="research" | inat$user=="frousseu",]



remotes::install_github("Rekyt/rtaxref")


library(rtaxref)
library(data.table)

#x<-as.data.table(rt_taxa_search(sciname = "Lolium arundinaceum"))
#x<-x[rankName=="Esp?ce",.(scientificName,referenceName)]
#x<-rt_taxa_fuzzymatch(sciname = "Benthamia chlorantha")
#x<-rt_taxa_search(sciname = "Benthamia chlorantha")
#x[,c("scientificName","referenceName")]

# remove "x" ?

species<-unique(inat$sp)
species<-species[-grep(" ? ",species)]
species<-sapply(strsplit(species," "),function(i){paste(i[1:2],collapse=" ")})
l<-lapply(species,function(i){
  print(match(i,species))
  x<-as.data.table(rt_taxa_search(sciname = i))
  if(ncol(x)==1){
    pow<-get_pow(i,ask=FALSE,accepted=TRUE,rank_filter="species")
    powid<-pow[1]
    syns<-pow_synonyms(powid)$name
    x<-rt_taxa_search(sciname = syns)
    as.data.table(x[,c("scientificName","referenceName")])
  }else{
    x[rankName=="Esp?ce",.(scientificName,referenceName)]
  }
})


as.data.table(rt_taxa_synonyms(id = x$id[2]))


library(taxize)
temp <- as.data.table(gnr_resolve(c("Gallus gallus")))
head(temp)

library(rgbif)
x<-as.data.table(name_backbone(name="Benthamia chlorantha", rank='species', kingdom='plants'))
head(x)

powo<-get_pow(sp,ask=FALSE,accepted=TRUE,rank_filter="species")
powourl<-data.frame(sp=sp,powo=attributes(powo)$uri)


pow<-get_pow("Benthamia chlorantha",ask=FALSE,accepted=TRUE,rank_filter="species")
powid<-pow[1]
syns<-pow_synonyms(powid)$name

x<-rt_taxa_search(sciname = syns)
x[,c("scientificName","referenceName")]

pow<-get_pow("Satorkis latisatis",ask=FALSE,accepted=TRUE,rank_filter="species")


library(taxize)
temp <- as.data.table(gnr_resolve(c("Circus hudsonicus")))
head(temp)


x<-as.data.table(name_backbone(name="Circus hudsonicus", rank='species'))
head(x)



d<-fread("C:/Users/God/Documents/predictors_sdm/inat/0137377-200613084148143.csv",select="species")

ed$species<-ed$scientific_name

name_backbone(name="Gallinago wilsoni", rank='species')



ed$species<-ed$scientific_name

w<-which(!ed$species%in%d$species)

l<-lapply(seq_along(w),function(i){
  cat(i,"/n")
  x<-name_backbone(name=ed$species[w[i]], rank='species')
  cbind(ed$species[w[i]],x[,c("scientificName","species","synonym")])
})
x<-rbindlist(l)


ed$species[]

########################
########################

library(readxl)

d<-as.data.frame(read_excel("C:/Users/God/Downloads/ITR_2020-1_Diffusion_Externe/ITR_2020-1_Diffusion_Externe/ITR-2020-1-MAJ-20201204_Diff-Externe.xlsx",skip=5))#range="A6:K1116"))
d<-d[,1:match("FAMILLE",names(d))]
d<-d[d$RANG>20,]

d$name<-d$"NOM BOTANIQUE"
d$accepted<-d$"NOM ACCEPTÉ (ou de rattachement)"
d$taxref<-d$"CODE TAXREF"

vars<-unique(c("tenella",sapply(strsplit(inat$sp," "),"[",3)))
vars<-vars[!is.na(vars)]


#vars<-grep("var. tenella|var. insularis",d$name)

d$name<-sub("^(\\S*\\s+\\S+).*", "\\1",d$name)
d$accepted<-sub("^(\\S*\\s+\\S+).*", "\\1",d$accepted)

l<-lapply(inat$sp,function(i){
  w<-which(i==d$name & is.na(d$accepted))  
  if(length(w)>0){
    sp<-d$name[w]
  }else{
    w<-which(i==d$name & !is.na(d$accepted)) 
    if(length(w)>0){
      sp<-d$accepted[w]
    }else{
      w<-which(i==d$accepted)
      if(length(w)>0){
        sp<-NA#d$accepted[w]
      }else{
        sp<-NA  
      }
    }
  }
  unique(sp)
})
stopifnot(unique(sapply(l,length))==1)
inat$cbnm<-unlist(l)
missing<-unique(inat$sp[is.na(inat$cbnm)])




match(missing,iucn$species)

x<-fromJSON("https://api.inaturalist.org/v1/taxa?q=Battus%20philenor")

o<-st_intersects(inat,mailles)


url<-paste0("https://api.checklistbank.org/dataset/139831/nameusage/https://www.inaturalist.org/taxa/",inat$id[1])

x<-fromJSON(url)


x<-fromJSON("https://api.checklistbank.org/dataset/139831/nameusage/https%3A%2F%2Fwww.inaturalist.org%2Ftaxa%2F49972")



x<-fromJSON(paste0(url,inat$id[match(sp,inat$sp)],"/related?datasetKey=2008"))

