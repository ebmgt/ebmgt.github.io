var n = location.pathname.indexOf("/",1);
var sub_domain = location.hostname.split('.').shift();
if (n == 0){n = location.pathname.length};
var pagename = location.pathname.split('/').slice(-1);
if (pagename == ""){pagename = "index.html"};
var repo_dir = location.pathname.substring(1,n);
var repo_name = repo_dir.replace(/\-/gi, ' '); 
var metagression = false;
var network = false;
var r_code = false;

function showtip(tiptext, trigger, width){
	$("#tip").css('display','block');
        $("#tip").html("<div style = 'background-color:white;opacity:1;border-style: solid; border-width: medium;padding:10px'>" + tiptext + '</div>');
	$("#tip").css('width', width);
	$("#tip").css({"background-color":"#6DC6E7"});
	$("#tip").css({"color":"#0022B4"});
	$("#tip").css({"opacity":"1"});
	var pos_left = trigger.position().left;
        if ((pos_left + $("#tip").width()) > $(window).width()){
            pos_left = $(window).width() - $("#tip").width() - 10;
		}
        if (pos_left < 0){
            pos_left = 10;
		}
	var pos_top = trigger.position().top;
        if ((pos_top + $("#tip").height()) > $(window).height()){
            pos_top = pos_top + 20 - $("#tip").height();
		}
        $("#tip").offset({top: pos_top, left: pos_left});
}  
$(document).ready(function(){
  //write footer business
  //write to div business in the footer
  //Resuse
  $("#business").append("<div style='text-align:center'><a href=\"https://github.com/" + sub_domain + '/' + repo_dir + "/tree/master/files/LICENSE.md\">License</a></div>")
  //Edit and issues/comments
  $("#business").append("<div style='text-align:center'><a href='https://github.com/" + sub_domain + '/' + repo_dir + "/blob/gh-pages/" + pagename + "'>Edit this page</a> - <a href='https://github.com/" + sub_domain + '/' + repo_dir + "/issues?q=is%3Aboth+is%3Aissue'>Issues and comments</a></div>")
  //Version date...
  lastmod = document.lastModified     // get string of last modified date
  lastmoddate = Date.parse(lastmod)   // convert modified string to date
  if (lastmoddate == 0) {               // unknown date (or January 1, 1970 GMT)
    $("#business").append("<div style='text-align:center'>You need a new browser</div>")
  } else {
    $("#business").append("<div style='text-align:center'>Updated: " + lastmod + " - <a href='https://github.com/" + sub_domain + '/' + repo_dir + "/commits/gh-pages/" + pagename + "'>History</a></div>")
  }
  //Event handlers
  $('#tip').mouseleave(function(event){
    $( "#tip" ).css('display', 'none');
    });
  $('a.hastip_intitle').mouseenter(function(event){
    var tiptext = $(this).attr('title')
    trigger = $(this);
    width = "200px"
    showtip(tiptext, trigger, width);
    });
  $('a.hastip').mouseenter(function(event){
    var tipname = $(this).attr("id");
    trigger = $(this);
    width = "400px"
    $("#tip").load("/tips.xml", function(responseTxt,statusTxt,xhr){
      if(statusTxt=="success"){
        //alert("Success: "+xhr.status+": "+xhr.statusText);
        var xmlDoc = $.parseXML(responseTxt)
        var tiptext = '';
        var temp =$(xmlDoc).find("[name= " + tipname + "]").each(function(){
          $(this).find('p').each(function(){
            tiptext += $(this).text() + "<br>"
          })
          $(this).find('href').each(function(){
            if ($(this).text().length> 5){
              tiptext += "From: <a href='" +  $(this).text() + "' style='align:center'>"
              $(this).find('text').each(function(){
                tiptext += $(this).text() + "</a>"
                })
              }
            })
             });
        showtip(tiptext, trigger, width)
      }
      if(statusTxt=="error"){
        alert("Error: "+xhr.status+": "+xhr.statusText);
      }
      });


  });
});
