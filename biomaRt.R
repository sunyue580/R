library(biomaRt)

##############################1、listEnsembl & listDatasets
listEnsembl()
listEnsembl(GRCh=37)  #报错？
listEnsembl(version=78)  #报错？

ensembl = useEnsembl(biomart="ensembl")
head(listDatasets(ensembl))   #列出物种

#
grch37 = useEnsembl(biomart="ensembl",GRCh=37)
listDatasets(grch37)
ensembl78 = useEnsembl(biomart="ensembl",version=100)  
listDatasets(ensembl78)[31:35,] 


#####################################2、useEnsembl
ensembl = useEnsembl(biomart="ensembl", dataset="hsapiens_gene_ensembl")
# chr1_genes <- getBM(attributes=c('ensembl_gene_id',
#                                    'ensembl_transcript_id','hgnc_symbol','chromosome_name','start_position','end_position'), filters =
#                         'chromosome_name', values ="1", mart = ensembl)
genes <- getBM(attributes=c('ensembl_gene_id',
                            'ensembl_transcript_id','hgnc_symbol','chromosome_name','start_position','end_position'), mart = ensembl)
genes <- genes[genes$chromosome_name %in% c(seq(1,22,1),"X","Y"),]
dim(genes) #231944      6

ensembl = useEnsembl(biomart="ensembl", dataset="hsapiens_gene_ensembl", GRCh=37)
ensembl = useEnsembl(biomart="ensembl", dataset="hsapiens_gene_ensembl", version=100)

##################################3、listMarts, listDatasets and useMart for the Ensembl mirrors
##三个镜像网址
# Ensembl US West: http://uswest.ensembl.org/index.html
# Ensembl US East: http://useast.ensembl.org/index.html
# Ensembl Asia: http://asia.ensembl.org/index.html
listMarts(host="uswest.ensembl.org")
ensembl_us_west = useMart(biomart="ENSEMBL_MART_ENSEMBL", host="uswest.ensembl.org")
head(listDatasets(ensembl_us_west))


################################4、listFilters & listAttributes
ensembl = useEnsembl(biomart="ensembl", dataset="hsapiens_gene_ensembl")
head(listFilters(ensembl))
head(listAttributes(ensembl))


#################################5、getBM
ensembl = useEnsembl(biomart="ensembl", dataset="hsapiens_gene_ensembl")
# chr1_genes <- getBM(attributes=c('ensembl_gene_id',
#                                    'ensembl_transcript_id','hgnc_symbol','chromosome_name','start_position','end_position'), filters =
#                         'chromosome_name', values ="1", mart = ensembl)
genes <- getBM(attributes=c('ensembl_gene_id',
                            'ensembl_transcript_id','hgnc_symbol','chromosome_name','start_position','end_position'), mart = ensembl)
genes <- genes[genes$chromosome_name %in% c(seq(1,22,1),"X","Y"),]
dim(genes) #231944      6

##过滤某个基因
ensembl = useEnsembl(biomart="ensembl", dataset="hsapiens_gene_ensembl")
hgnc_swissprot <- getBM(attributes=c('ensembl_gene_id','ensembl_transcript_id','hgnc_symbol','uniprotswissprot'),filters = 'ensembl_gene_id', values = 'ENSG00000139618', mart = ensembl)
hgnc_swissprot

#####植物基因名
#在ensemble plants上能看到所有已提交的物种信息
ensembl = useMart(biomart = "plants_mart",host = "http://plants.ensembl.org")
#查看ensemble plants都有哪些物种信息，并设置为该物种信息。
dataset <- listDatasets(mart = ensembl)
head(dataset)
grep("Sorghum",dataset$description)
dataset[74,]
ensembl = useMart(biomart = "plants_mart",host = "http://plants.ensembl.org",dataset="sbicolor_eg_gene")
#查看该dataset上都有哪些属性，方便后面做添加
attributes <- listAttributes(ensembl)
genes <- getBM(attributes =c("entrezgene_id",'external_gene_name',"description"),mart = ensembl)




