/*
    GSCALite main js
    Author: C.J. Liu
    Contact: chunjie-sam-liu@foxmail.com
    This file is for shinyjs to extend
*/

shinyjs.init = function(){
  // all checked after initiation
  $("button.btn.checkbtn.btn-default").removeClass("active").addClass("active");
  $('input[type="checkbox"]').prop("checked", true);
};

shinyjs.example_gene_set = function(params){
  var defaultParams = {id: null};
  params = shinyjs.getParams(params, defaultParams);
  var selector = $("#" + params.id);
  selector.val("A2M ACE ANGPT2 BPI CD1B CDR1 EGR2 EGR3 HBEGF HERPUD1 MCM2 MRE11A PCTP PODXL PPAP2B PPY PTGS2 RCAN1 SLC4A7 THBD");
};

shinyjs.checkall = function() {
  $("button.btn.checkbtn.btn-default").removeClass("active").addClass("active");
  $('input[type="checkbox"]').prop("checked", true);
};

shinyjs.uncheckall = function() {
  $("button.btn.checkbtn.btn-default").removeClass("active");
  $('input[type="checkbox"]').prop("checked", false);
};

shinyjs.switch = function(params){
  var defaultParams = {id: null};
  params = shinyjs.getParams(params, defaultParams);
  var selector = $("#" + params.id);
  selector.change();
};