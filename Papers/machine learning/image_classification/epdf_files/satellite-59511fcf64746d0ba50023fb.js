if(!(_satellite.readCookie("s_pn"))){
	_satellite.setCookie("s_pn",0);
}
if(_satellite.readCookie("s_pn")){
	s_pn = parseInt(_satellite.readCookie("s_pn")) + 1;
	_satellite.setCookie("s_pn",s_pn);
	_satellite.setVar("pn",s_pn);
}
else{
	_satellite.setVar("pn","nocookie");
}