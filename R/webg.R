



webgestalt<-function(
                genset
                ,genbkg=''
                ,idtype='genesymbol'
                ,organism='hsapiens'
                ,enrichDatabase=''
                ,method='ORA'
                # outdir=getwd()
                ,outdir=getwd(),
                fdrThr=0.1  
                    ){

    if(method=='ORA'){
        result=WebGestaltR(
        enrichMethod=method                   ##  options=c('ORA','GSEA')
        ,organism=organism                  ##  listOrganism -- to check the available organisms
        ,enrichDatabase=ityp                ##  listGeneSet -- to check the available functional databases for the selected organism
                
        ,interestGene=genset                ##  Users can also use the R object as the input. If ‘enrichMethod’ is ‘ORA’, ‘interestGene’ should be an R ‘vector’ object containing the interesting gene list. If ‘enrichMethod’ is ‘GSEA’, ‘interestGene’ should be an R ‘data.frame’ object containing two columns: the gene list and the corresponding scores.
    ## df - column 1 - genes, column 2 - rank metric
        ,interestGeneType=idtype              ##  supported ID type of the WebGestaltR for the selected organism can be found by the function ‘listIDType’

        ,referenceGene=genbkg               ##  For ORA method, users can also use the R object as the reference gene list.  ‘referenceGene’ should be an R ‘vector’ object containing the reference gene list.
        ,referenceGeneType=idtype             ##  The ID type of the reference gene list. The supported ID type of the WebGestaltR for the selected organism can be found by the function ‘listIDType’.  If the ‘organism’ is ‘others’, users do not need to set this parameter.

    ##  commented=='default'    --------------------------------------------------------------------------------------------------------------------------------
        # ,enrichDatabaseFile=NULL                  ##  If users set ‘organism’ as ‘others’ or set ‘enrichDatabase’ as ‘others’, users need to upload a GMT file as the functional categories for the enrichment analysis. The extension of the file should be ‘gmt’ and the first column of the file is the category ID, the second one is the external link for the category. Genes annotated to the category are from the third column.  All columns are separated by tab.
        # ,enrichDatabaseType=NULL                  ##  If users set ‘enrichDatabase’ as ‘others’ ...
        # ,enrichDatabaseDescriptionFile=NULL             ##  Users can also upload a description file for the uploaded ‘enrichDatabaseFile’
        # ,interestGeneFile=NULL                    ##  If ‘enrichMethod’ is ‘ORA’, the extension of the ‘interestGeneFile’ should be ‘txt’ and the file can only contain one column: the interesting gene list. If ‘enrichMethod’ is ‘GSEA’, the extension of the ‘interestGeneFile’ should be ‘rnk’ and the file should contain two columns separated by tab: the gene list and the corresponding scores
        #
        ,collapseMethod="mean"                  ##  The method to collapse the duplicate ids for the GSEA method. ‘mean’, ‘median’, ‘min’ and ‘max’ represent the mean, median, minimum and maximum of scores for the duplicate ids.
        # ,referenceGeneFile=NULL                 ##  For ORA method, the users need to upload the reference gene list. The extension of the ‘referenceGeneFile’ should be ‘txt’ and the file can only contain one column: the reference gene list
        # ,referenceSet=NULL                    ##  GSEA >> ‘listReferenceSet’ -- Users can directly select the reference set from the existing platform in the WebGestaltR and do not need to upload the reference set. All existing platform supported in the WebGestaltR can be found by the function ‘listReferenceSet’.  If ‘referenceGeneFile’ and ‘refereneceGene’ are \ codeNULL, WebGestaltR will use the ‘referenceSet’ as the reference gene set. Otherwise, WebGestaltR will use the user uploaded reference set for the enrichment analysis.
        # ,minNum=10                          ##  WebGestaltR will exclude the categories with the number of annotated genes less than ‘minNum’ for the enrichment analysis. Default=10
        ,maxNum=2000                        ##  WebGestaltR will exclude the categories with the number of annotated genes larger than ‘maxNum’ for the enrichment analysis. The default is ‘500’
        # ,fdrMethod="BH"                   ##  ORA >> FDR methods: ‘holm’, ‘hochberg’, ‘hommel’, ‘bonferroni’, ‘BH’ and ‘BY’. The default is ‘BH’.
        # ,sigMethod="fdr"                  ##  Two significant methods are available in the WebGestaltR: ‘fdr’ and ‘top’. ‘fdr’ means the enriched categories are identified based on the FDR and ‘top’ means all categories are ranked based on FDR and then selected top categories as the enriched categories. The default is ‘fdr’
        ,fdrThr=fdrThr                    ##  The significant level for the ‘fdr’ method. The default is ‘0.05’.
        # ,topThr=10                          ##  The threshold for the ‘top’ method. The default is ‘10’
        # ,dNum=20                          ##  The number of enriched categories visualized in the final report.  The default is ‘20’ and the maximum is ‘100’. A larger ‘dNum’ will increase the running time
        # ,perNum=1000                        ##  The number of permutations for the GSEA method. The default is ‘1000’.
        # ,lNum=20                          ##  The number of categories with the output leading edge genes for the GSEA method.  The default is ‘20’.  ‘Note’: GSEA first ranks the categories based on NES (normalized enrichment score) instead of FDR and then outputs the leading edge genes for top ‘lNum’ categories. Because NES does not necessarily decrease with the increase of the FDR, using ‘sigMethod’ defined in WebGestaltR to identify the significant categories may cause some categories with outputted leading edge genes are not included in the final result even if the number of significant categories is larger than ‘lNum’.
        ,isOutput=F                        ##  If ‘is.output’ is TRUE, WebGestaltR will create a folder named by the ‘projectName’ and save the mapping results, GO slim summary, enrichment results and an user-friendly HTML report in the folder. Otherwise, WebGestaltR will only return an R ‘data.frame’ object containing the enrichment results. If hundreds of gene list need to be analyzed simultaneous, it is better to set ‘is.output’ as FALSE.
        ,outputDirectory=outdir                   ##  The output directory for the results
        # ,projectName=NULL                     ##  The name of the project. If ‘projectName’ is Null, WebGestaltR will use time stamp as the project name
        # ,keepGSEAFolder=FALSE                   ##  If ‘keepGSEAFolder’ is TRUE, WebGestaltR will keep all folders generated from GSEA tool that contain all figures and tables related to the GSEA analysis
        # ,methodType="R"                       ##  For the large ID mapping table (e.g. dbSNP mapping table), Users can use ‘R’ or ‘Python’ function to read it.  Sometimes ‘Python’ code is faster than the ‘R’ code.  If users use ‘Python’ code to read the mapping table, users should first install python and the module ‘pandas’ in the computer
        # ,dagColor="binary"                      ##  If ‘dagColor’ is binary, the significant terms in the DAG structure will be colored by red for ORA method or red (positive related) and blue (negative related) for GSEA method. If ‘dagColor’ is continous, the significant terms in the DAG structure will be colored by the red gradient for ORA method or red (positive related) and blue (negative related) gradient for GSEA method.based on the corresponding FDR
        # ,hostName="http://www.webgestalt.org/"            ##  The server URL for accessing the data. User can use ‘listArchiveURL’ function to get all archive version URL
        )
    }
    if(method=='GSEA'){
        result=WebGestaltR(
        enrichMethod=method                   ##  options=c('ORA','GSEA')
        ,organism=organism                  ##  listOrganism -- to check the available organisms
        ,enrichDatabase=ityp                ##  listGeneSet -- to check the available functional databases for the selected organism
                
        ,interestGene=genset                ##  Users can also use the R object as the input. If ‘enrichMethod’ is ‘ORA’, ‘interestGene’ should be an R ‘vector’ object containing the interesting gene list. If ‘enrichMethod’ is ‘GSEA’, ‘interestGene’ should be an R ‘data.frame’ object containing two columns: the gene list and the corresponding scores.
    ## df - column 1 - genes, column 2 - rank metric
        ,interestGeneType=idtype              ##  supported ID type of the WebGestaltR for the selected organism can be found by the function ‘listIDType’

        # ,referenceGene=genbkg               ##  For ORA method, users can also use the R object as the reference gene list.  ‘referenceGene’ should be an R ‘vector’ object containing the reference gene list.
        # ,referenceGeneType=idtype             ##  The ID type of the reference gene list. The supported ID type of the WebGestaltR for the selected organism can be found by the function ‘listIDType’.  If the ‘organism’ is ‘others’, users do not need to set this parameter.

    ##  commented=='default'    --------------------------------------------------------------------------------------------------------------------------------
        # ,enrichDatabaseFile=NULL                  ##  If users set ‘organism’ as ‘others’ or set ‘enrichDatabase’ as ‘others’, users need to upload a GMT file as the functional categories for the enrichment analysis. The extension of the file should be ‘gmt’ and the first column of the file is the category ID, the second one is the external link for the category. Genes annotated to the category are from the third column.  All columns are separated by tab.
        # ,enrichDatabaseType=NULL                  ##  If users set ‘enrichDatabase’ as ‘others’ ...
        # ,enrichDatabaseDescriptionFile=NULL             ##  Users can also upload a description file for the uploaded ‘enrichDatabaseFile’
        # ,interestGeneFile=NULL                    ##  If ‘enrichMethod’ is ‘ORA’, the extension of the ‘interestGeneFile’ should be ‘txt’ and the file can only contain one column: the interesting gene list. If ‘enrichMethod’ is ‘GSEA’, the extension of the ‘interestGeneFile’ should be ‘rnk’ and the file should contain two columns separated by tab: the gene list and the corresponding scores
        #
        ,collapseMethod="mean"                  ##  The method to collapse the duplicate ids for the GSEA method. ‘mean’, ‘median’, ‘min’ and ‘max’ represent the mean, median, minimum and maximum of scores for the duplicate ids.
        # ,referenceGeneFile=NULL                 ##  For ORA method, the users need to upload the reference gene list. The extension of the ‘referenceGeneFile’ should be ‘txt’ and the file can only contain one column: the reference gene list
        # ,referenceSet=NULL                    ##  GSEA >> ‘listReferenceSet’ -- Users can directly select the reference set from the existing platform in the WebGestaltR and do not need to upload the reference set. All existing platform supported in the WebGestaltR can be found by the function ‘listReferenceSet’.  If ‘referenceGeneFile’ and ‘refereneceGene’ are \ codeNULL, WebGestaltR will use the ‘referenceSet’ as the reference gene set. Otherwise, WebGestaltR will use the user uploaded reference set for the enrichment analysis.
        # ,minNum=10                          ##  WebGestaltR will exclude the categories with the number of annotated genes less than ‘minNum’ for the enrichment analysis. Default=10
        ,maxNum=2000                        ##  WebGestaltR will exclude the categories with the number of annotated genes larger than ‘maxNum’ for the enrichment analysis. The default is ‘500’
        # ,fdrMethod="BH"                   ##  ORA >> FDR methods: ‘holm’, ‘hochberg’, ‘hommel’, ‘bonferroni’, ‘BH’ and ‘BY’. The default is ‘BH’.
        # ,sigMethod="fdr"                  ##  Two significant methods are available in the WebGestaltR: ‘fdr’ and ‘top’. ‘fdr’ means the enriched categories are identified based on the FDR and ‘top’ means all categories are ranked based on FDR and then selected top categories as the enriched categories. The default is ‘fdr’
        ,fdrThr=fdrThr                    ##  The significant level for the ‘fdr’ method. The default is ‘0.05’.
        # ,topThr=10                          ##  The threshold for the ‘top’ method. The default is ‘10’
        # ,dNum=20                          ##  The number of enriched categories visualized in the final report.  The default is ‘20’ and the maximum is ‘100’. A larger ‘dNum’ will increase the running time
        # ,perNum=1000                        ##  The number of permutations for the GSEA method. The default is ‘1000’.
        # ,lNum=20                          ##  The number of categories with the output leading edge genes for the GSEA method.  The default is ‘20’.  ‘Note’: GSEA first ranks the categories based on NES (normalized enrichment score) instead of FDR and then outputs the leading edge genes for top ‘lNum’ categories. Because NES does not necessarily decrease with the increase of the FDR, using ‘sigMethod’ defined in WebGestaltR to identify the significant categories may cause some categories with outputted leading edge genes are not included in the final result even if the number of significant categories is larger than ‘lNum’.
        ,isOutput=F                        ##  If ‘is.output’ is TRUE, WebGestaltR will create a folder named by the ‘projectName’ and save the mapping results, GO slim summary, enrichment results and an user-friendly HTML report in the folder. Otherwise, WebGestaltR will only return an R ‘data.frame’ object containing the enrichment results. If hundreds of gene list need to be analyzed simultaneous, it is better to set ‘is.output’ as FALSE.
        ,outputDirectory=outdir                   ##  The output directory for the results
        # ,projectName=NULL                     ##  The name of the project. If ‘projectName’ is Null, WebGestaltR will use time stamp as the project name
        # ,keepGSEAFolder=FALSE                   ##  If ‘keepGSEAFolder’ is TRUE, WebGestaltR will keep all folders generated from GSEA tool that contain all figures and tables related to the GSEA analysis
        # ,methodType="R"                       ##  For the large ID mapping table (e.g. dbSNP mapping table), Users can use ‘R’ or ‘Python’ function to read it.  Sometimes ‘Python’ code is faster than the ‘R’ code.  If users use ‘Python’ code to read the mapping table, users should first install python and the module ‘pandas’ in the computer
        # ,dagColor="binary"                      ##  If ‘dagColor’ is binary, the significant terms in the DAG structure will be colored by red for ORA method or red (positive related) and blue (negative related) for GSEA method. If ‘dagColor’ is continous, the significant terms in the DAG structure will be colored by the red gradient for ORA method or red (positive related) and blue (negative related) gradient for GSEA method.based on the corresponding FDR
        # ,hostName="http://www.webgestalt.org/"            ##  The server URL for accessing the data. User can use ‘listArchiveURL’ function to get all archive version URL
        )
    }
    return(result)
}



webg<-function(
                genset
                ,genbkg=''
                ,quick=F
                ,idtype='genesymbol'
                ,organism='hsapiens'
                ,enrichtype=c('go','pathw')
                ,method='ORA'
                # outdir=getwd()
                ,outdir=getwd(),
                fdrThr=0.1) {
##  organism - currently expects only hsapiens or mmusculus, some things may break otherwise
##  idtype - same for geneSet and genbkg, assume that the user is using well mapped & related gene sets
##  WebGestaltR(options) - code currently disregarding non hard-coded choices
##  enrichtype - shorthand for terms is translated into query terms for WebGestalt, alternatively use full names as in WebGestalt. mixing is not supported ie NOT c('go','pathway_Reactome')
##  idtype - typically used: "genesymbol","ensembl_gene_id","entrezgene"

Library('WebGestaltR')


  if(!is.vector(genset)&method=='ORA'){
    stop(paste0('class(genset) is ',class(genset),', expect vector'))
  }


if(sum(enrichtype%in%c('go','pathw','other'))>0){
##  translate base keywords into WebGestalt query terms    --------------------------------------------------------------------------------------------------------------------------------
full=c("geneontology_Biological_Process_noRedundant","geneontology_Cellular_Component_noRedundant","geneontology_Molecular_Function_noRedundant","pathway_KEGG","pathway_Panther","pathway_Reactome","pathway_Wikipathway","network_PPI_BIOGRID","network_miRNA_target","network_Transcription_Factor_target","chromosomalLocation_CytogenicBand","geneontology_Biological_Process","geneontology_Cellular_Component","geneontology_Molecular_Function","network_Kinase_target","disease_Disgenet","disease_GLAD4U","disease_OMIM","drug_DrugBank","drug_GLAD4U","phenotype_Human_Phenotype_Ontology","phenotype_Mammalian_Phenotype_Ontology")
termkey=as.data.frame(full)
termkey$spp=c("any","any","any","any","any","any","any","any","any","any","any","any","any","any","hsapiens","hsapiens","hsapiens","hsapiens","hsapiens","hsapiens","hsapiens","mmusculus")
termkey$term=c("go","go","go","pathw","pathw","pathw","pathw","other","other","other","other","other","other","other","other","other","other","other","other","other","other","other")

if(quick){termkey=termkey[c(1,6,16),]}

##  abridged version    --------------------------------------------------------------------------------------------------------------------------------
termkey=termkey[!termkey$full%in%c("chromosomalLocation_CytogenicBand","geneontology_Biological_Process","geneontology_Cellular_Component","geneontology_Molecular_Function","network_Kinase_target","drug_GLAD4U","network_PPI_BIOGRID","network_miRNA_target"),]


full=enrichtype[enrichtype%in%full]
enrichtype=termkey[termkey$term%in%enrichtype & termkey$spp%in%c(organism,'any'),'full']
}

cat('\nterms selected:\n')
print(enrichtype)
##  hardcode currently available species/enrichments to avoid wasting time connecting every time..
  if(method=='ORA'&length(genbkg)==1){
    if(genbkg==''){
      stop('for enrichment method="ORA", provide a background gene set or use method="GSEA"')
    }
  }

  if(method=='GSEA'){
    if(ncol(genset)!=2){
      stop('for enrichment method="GSEA", provide a genset with 2 columns - gene ids, rank metric')
    }
  }


  if(!(organism%in%c("athaliana","btaurus","celegans","cfamiliaris","drerio","sscrofa","dmelanogaster","ggallus","hsapiens","mmusculus","rnorvegicus","scerevisiae"))){
    if(!(organism%in%listOrganism() )){
      stop(paste0('"organism" ',organism,' - not supported by WebGestalt, use "listOrganism()" to find check for correct name'))
    }
  }


  # if(organism=='mmusculus'){
  #   enrichal=c("geneontology_Biological_Process","geneontology_Cellular_Component","geneontology_Molecular_Function","geneontology_Biological_Process_noRedundant","geneontology_Cellular_Component_noRedundant","geneontology_Molecular_Function_noRedundant","pathway_KEGG","pathway_Panther","pathway_Reactome","pathway_Wikipathway","network_miRNA_target","network_PPI_BIOGRID","network_Transcription_Factor_target","chromosomalLocation_CytogenicBand")
  #   if(sum(enrichtype%in%enrichal)!=length(enrichtype)){
  #     enrichal=listGeneSet(organism="mmusculus")
  #     if(sum(enrichtype%in%enrichal)!=length(enrichtype)){
  #       stop(paste0('"enrichtype" ',paste(enrichtype[!enrichtype%in%enrichal],collapse=', '),' - not supported by WebGestalt, use "listGeneSet(organism="relevant_species")"'))
  #     }
  #   }

  #   idtypal=c("ABI_550XL_Wildfire","ABI_SOLiD_Exon","ABI_SOLiD_Klaus","ABI_SOLiD","affy_ClariomD-MTA-1_0.na36.mm10","affy_HTMG-430PM_GPL11180","affy_mg_u74a","affy_mg_u74av2","affy_mg_u74b","affy_mg_u74bv2","affy_mg_u74c","affy_mg_u74cv2","affy_microRNA4.0_GPL21572","affy_moe430a","affy_moe430b","affy_moex_1_0_st_v1","affy_mogene_1_0_st_v1","affy_mogene_2_1_st_v1","affy_mouse430_2","affy_mouse430a_2","affy_mu11ksuba","affy_mu11ksubb","agilent_Custom_GPL2510","agilent_sureprint_g3_ge_8x60k","agilent_ToxicogenomicsG4121A_GPL891","agilent_wholegenome_4x44k_v1","agilent_wholegenome_4x44k_v2","codelink","dbSNP","embl","ensembl_gene_id","ensembl_peptide_id","entrezgene_protein-coding","entrezgene","genename","genesymbol","illumina_mouseref_8_v2","illumina_mousewg_6_v1","illumina_mousewg_6_v2","illumina_ScriptSeq_RNASeqV2","illumina_WG6_Customarray","interpro","IonTorrent_RNASeqMature","MGI","Protein_GeneChip34","Protein_GeneChip41","Protein_GeneChip51","protein_id","refseq_mrna","refseq_peptide","unigene","uniprot_swissprot")
  #   if(!(idtype%in%idtypal)){
  #     idtypal=listIDType(organism="mmusculus")
  #     if(!(idtype%in%idtypal)){
  #       stop(paste0(paste('"idtype" ',idtype[!idtype%in%idtypal],collapse=', '),' - not supported by WebGestalt, use "listIDType(organism="relevant_species")"'))
  #     }
  #   }

  # }

  # if(organism=='hsapiens'){
  #   enrichal=c("geneontology_Biological_Process_genesymbol","geneontology_Biological_Process","geneontology_Cellular_Component","geneontology_Molecular_Function","geneontology_Biological_Process_noRedundant","geneontology_Cellular_Component_noRedundant","geneontology_Molecular_Function_noRedundant","pathway_KEGG","pathway_Panther","pathway_Reactome","pathway_Wikipathway","network_miRNA_target","network_PPI_BIOGRID","network_Transcription_Factor_target","phenotype_Mammalian_Phenotype_Ontology")
  #   if(sum(enrichtype%in%enrichal)!=length(enrichtype)){
  #     enrichal=listGeneSet(organism="hsapiens")
  #     if(sum(enrichtype%in%enrichal)!=length(enrichtype)){
  #       stop(paste0('"enrichtype" ',paste(enrichtype[!enrichtype%in%enrichal],collapse=', '),' - not supported by WebGestalt, use "listGeneSet(organism="relevant_species")"'))
  #     }
  #   }

  #   idtypal=c("affy_Axiom_BioBank1.na35_probe","affy_Axiom_BioBank1.na35_rsid","affy_Axiom_PMRA.na35_probe","affy_Axiom_PMRA.na35_rsid","affy_Axiom_tx_v1.na35_probe","affy_Axiom_tx_v1.na35_rsid","affy_GenomeWideSNP_5.na35_SNP_probe","affy_GenomeWideSNP_5.na35_SNP_rsid","affy_GenomeWideSNP_6.na35_SNP_probe","affy_GenomeWideSNP_6.na35_SNP_rsid","affy_hc_g110","affy_hg_focus","affy_hg_u133_plus_2","affy_hg_u133a_2","affy_hg_u133a","affy_hg_u133b","affy_hg_u95a","affy_hg_u95av2","affy_hg_u95b","affy_hg_u95c","affy_hg_u95d","affy_hg_u95e","affy_hta_2_0","affy_huex_1_0_st_v2","affy_hugene_1_0_st_v1","affy_hugene_2_0_st_v1","affy_hugenefl","affy_Mapping10K_Xba142.na32_SNP_probe","affy_Mapping10K_Xba142.na32_SNP_rsid","affy_Mapping250K_Nsp.na32_SNP_probe","affy_Mapping250K_Nsp.na32_SNP_rsid","affy_Mapping250K_Sty.na32_SNP_probe","affy_Mapping250K_Sty.na32_SNP_rsid","affy_Mapping50K_Hind240.na32_SNP_probe","affy_Mapping50K_Hind240.na32_SNP_rsid","affy_Mapping50K_Xba240.na32_SNP_probe","affy_Mapping50K_Xba240.na32_SNP_rsid","affy_OncoScan.na33.r1_SNP_probe","affy_OncoScan.na33.r1_SNP_rsid","affy_primeview","affy_RosettaMerck_Human_RSTA","affy_u133_x3p","agilent_cgh_44b","agilent_custom_SAGE_Bionetworks_GPL564","agilent_human_custom_GPL564","agilent_sureprint_g3_ge_8x60k_v2","agilent_sureprint_g3_ge_8x60k","agilent_wholegenome_4x44k_v1","agilent_wholegenome_4x44k_v2","codelink","dbSNP","embl","ensembl_gene_id","ensembl_peptide_id","entrezgene_protein-coding","entrezgene","genename","genesymbol","illumina_human_methylation_27","illumina_human_methylation_450","illumina_human-6v3.0_expression_beadchip","illumina_humanht_12_v3","illumina_humanht_12_v4","illumina_humanref_8_v3","illumina_humanRef-8v2.0_expression_beadchip_GPL6104","illumina_humanwg_6_v1","illumina_humanwg_6_v2","illumina_humanwg_6_v3","illumina_Infinium_HumanMethylation_beadchip","illumina_Sentrix_HumanRef-8v2_GPL2700","interpro","protein_id","refseq_mrna","refseq_peptide","The_Genotype-Tissue_ExpressionProjectGTEx","unigene","uniprot_swissprot")
  #   if(!(idtype%in%idtypal)){
  #     idtypal=listIDType(organism="hsapiens")
  #     if(!(idtype%in%idtypal)){
  #       stop(paste0(paste('"idtype" ',idtype[!idtype%in%idtypal],collapse=', '),' - not supported by WebGestalt, use "listIDType(organism="relevant_species")"'))
  #     }
  #   }

  # }

setlist=listGeneSet(organism="hsapiens")$name
overlap(setlist,enrichtype)


  print(str(genset))
  print(str(genbkg))
  enrichlis=list()
  for(ityp in enrichtype[enrichtype%in%setlist]){
    lprogr(ityp,enrichtype,T)
    enrichlis[[ityp]]=as.data.frame('')   ## create blank space

    

        tryCatch({
        # Just to highlight: if you want to use more than one
        # R expression in the "try" part then you'll have to
        # use curly brackets.
        # 'tryCatch()' will return the last evaluated expression
        # in case the "try" part was completed successfully

        enrichlis[[ityp]]=webgestalt(
                    genset=genset
                    ,genbkg=''
                    ,idtype=idtype
                    ,organism=organism
                    ,enrichtype=ityp
                    ,method=method
                    # outdir=getwd()
                    ,outdir=getwd(),
                    fdrThr=0.1
                )                                                                                                                ##  main wrapper funciton to connect to "WebGestalt" server, requires "WebgestaltR" package                                            genset=mgens[[iset]]                                                                                      ##  character list of gene_ids to perform enrichment with                                            ,genbkg=bkg_genes                                                                                                        ##  backround genes, ie genes used to perform the analysis that yielded "genset" above, eg, all expressed in the assay/experiment                                            ,idtype='genesymbol'                                                                                     ##  idtype - same for geneset and genbkg, assume that the user is using well mapped & related gene sets. typically used: "genesymbol","ensembl_gene_id","entrezgene"                                            ,enrichtype = enrich_terms                                                                          ##  can specify general categories or use list of specific enrichment terms (as per website / within function code)                                            ,organism = "hsapiens"                                            ,quick=F                                                                                                                               ##  limit to 4 pre-selected terms geneontology_Biological_Process_noRedundant, pathway_KEGG, pathway_Reactome, disease_Disgenet                                            )
        # The return value of `readLines()` is the actual value
        # that will be returned in case there is no condition
        # (e.g. warning or error).
        # You don't need to state the return value via `return()` as code
        # in the "try" part is not wrapped insided a function (unlike that
        # for the condition handlers for warnings and error below)
        },
        error=function(cond) {
        message(paste("URL does not seem to exist:", url))
        message("Here's the original error message:")
        message(cond)
        # Choose a return value in case of error
        # return(NA)
        return('')
        },
        warning=function(cond) {
        message(paste("URL caused a warning:", url))
        message("Here's the original warning message:")
        message(cond)
        # Choose a return value in case of warning
        # return(NULL)
        return('')
        },
        finally={
        # NOTE:
        # Here goes everything that should be executed at the end,
        # regardless of success or error.
        # If you want more than one expression to be executed, then you
        # need to wrap them in curly brackets ({...}); otherwise you could
        # just have written 'finally=<expression>'
         enrichlis[[ityp]]=webgestalt(
                    genset=genset
                    ,genbkg=''
                    ,idtype=idtype
                    ,organism=organism
                    ,enrichtype=ityp
                    ,method=method
                    # outdir=getwd()
                    ,outdir=getwd(),
                    fdrThr=0.1
                )  
            
        ##  main wrapper funciton to connect to "WebGestalt" server, requires "WebgestaltR" package            genset=mgens[[iset]]                                                                                      ##  character list of gene_ids to perform enrichment with            ,genbkg=bkg_genes                                                                                                        ##  backround genes, ie genes used to perform the analysis that yielded "genset" above, eg, all expressed in the assay/experiment            ,idtype='genesymbol'                                                                                     ##  idtype - same for geneset and genbkg, assume that the user is using well mapped & related gene sets. typically used: "genesymbol","ensembl_gene_id","entrezgene"            ,enrichtype = enrich_terms                                                                          ##  can specify general categories or use list of specific enrichment terms (as per website / within function code)            ,organism = "hsapiens"            ,quick=F                                                                                                                               ##  limit to 4 pre-selected terms geneontology_Biological_Process_noRedundant, pathway_KEGG, pathway_Reactome, disease_Disgenet            )
        message("Some other message at the end")
        }


  ##  may need to add 'no enrichment' for used terms that did not get a list when returned from WebGestaltR
  cat('\n')
  }

  return(invisible(enrichlis))
}


 


webg.plot<-function(webg_list,do_plots=F,zero_replace=1e-20,p_thresh=0.1){
#  warning('NOTE: this function is designed for 2+ modules i.e. list(M1=webg_output,M2=webg_output)')
  if(length(webg_list)>1){
    cat('\tplot heatmap of multiple "modules"\n')
    enricht=unique(unlist(lapply(webg_list,names)))
    enrich_dat=list()
    pdat=list()
    for(iter in enricht){
      lprogr(iter,enricht,T)
      molder=''
      for(imod in names(webg_list)){
        cat('\t\t',imod)

        if(!is.null(webg_list[[imod]][[iter]])){

        if('description'%in%colnames(webg_list[[imod]][[iter]])){
          holder=webg_list[[imod]][[iter]][,'FDR',drop=F]
            colnames(holder)=imod
            # rownames(holder)=webg_list[[imod]][[iter]][,'description']
            rownames(holder)=paste0(webg_list[[imod]][[iter]][,'geneSet'],';   ',webg_list[[imod]][[iter]][,'description'])  ##  
##  GSEA - can produce erros like the below
#          geneSet                               description
# 139 R-MMU-427413 NoRC negatively regulates rRNA expression
# 211 R-MMU-573389 NoRC negatively regulates rRNA expression
        }
        if(!'description'%in%colnames(webg_list[[imod]][[iter]])&'geneSet'%in%colnames(webg_list[[imod]][[iter]])){
          holder=webg_list[[imod]][[iter]][,'FDR',drop=F]
            colnames(holder)=imod
            # rownames(holder)=webg_list[[imod]][[iter]][,'geneSet']
        }

        molder=rmerg(molder,holder,verbose=F)
        cat('\t',max(min(holder),spval(holder,value=2,mode='min'),na.rm=T),'\n')    ## extract the lowest p-value in this set
        }

##  when one of the gene lists is very small
        # if(length(webg_list[[imod]][[iter]])==0){
        #   holder=as.data.frame(1)
        #     colnames(holder)=imod
        #   molder=rmerge(molder,holder,verbose=F)
        # }
        if(length(webg_list[[imod]][[iter]])==1){
          if(webg_list[[imod]][[iter]]=="ERROR: All genes in the uploaded gene list are not annotated to any category of the functional database."){ ## breaks if is.null()==T
            holder=as.data.frame(1)
              colnames(holder)=imod
            molder=rmerge(molder,holder,verbose=F)
            cat('\t no enrichment found\n')   ## extract the lowest p-value in this set
          }
        }
        if(is.null(webg_list[[imod]][[iter]])){
          holder=as.data.frame(1)
            colnames(holder)=imod
          molder=rmerge(molder,holder,verbose=F)
          cat('\t no enrichment found\n')   ## extract the lowest p-value in this set
        }
      }
      molder=molder[rownames(molder)!='1',colnames(molder)!='x']
      molder[is.na(molder)]=1       ##  missing == no enrichment => 1
      molder[molder==0]=1e-20       ##  p-value==0 is not viable for plotting => set as "zero_replace"
##  remove any rows where no value passes the p_threshold (ie all are "suggestive" enrichments)
      molder=molder[apply(molder,1,function(x){sum(x<p_thresh)>0}),]
      molder=molder[do.call(order,(lapply(1:NCOL(molder),function(i)molder[,i]))),]
      enrich_dat[[iter]]=make.numeric(molder)
      if(do_plots){
        return(gestaltheat(enrich_dat[[iter]]))   ##  need to add mtext description for enrichment type
      }
    }
  return(enrich_dat)
  }
}


