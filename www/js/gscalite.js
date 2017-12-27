/*
    GSCALite main js
    Author: C.J. Liu
    Contact: chunjie-sam-liu@foxmail.com
    This file is for shinyjs to extend
*/

shinyjs.init = function(){
  // all checked after initiation
  $('input[type="checkbox"]').prop("checked", true);
};

shinyjs.example_gene_set = function(params){
  var defaultParams = {id: null};
  params = shinyjs.getParams(params, defaultParams);
  var selector = $("#" + params.id);
  selector.val("ATP2A1 ATP2A2 ATP2A3 B4GALT1 CCND1 CREBBP E2F1 E2F3 EIF2C1 EIF2C2 EIF2C3 EIF2C4 EP300 FURIN JUN KAT2A KAT2B LFNG LOC441488 LOC728030 MAML1 MAML2 MAML3 MAMLD1 MFNG MOV10 NOTCH2 NOTCH3 NOTCH4 POFUT1 POGLUT1 RAB6A RBPJ RFNG SEL1L SNW1 ST3GAL3 ST3GAL6 TFDP1 TMED2 TNRC6A TNRC6B TNRC6C TP53");
};
