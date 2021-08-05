# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

# getLAI
function(par.trans, par.total, Time, eladp, lat_dec, long_dec) {
	## require(RAtmosphere)
	zenith <- SZA(Time, Lat=lat_dec, Lon=long_dec)*pi/180
	K <- sqrt(eladp^2 + tan(2*zenith))/(1.74 + 0.45*eladp + 0.1223*eladp^2 -
				0.013*eladp^3 + 0.000509*eladp)
	if(!is.vector(par.trans)) par.trans <- rowMeans(par.trans)
	if(!is.vector(par.total)) par.total <- rowMeans(par.total)
	LAI <- -log(par.trans/par.total)/K
	return(LAI)
}


#SZA
function(timein=Sys.time(),Lat = 50.910335,Lon = 11.568740){
# Calculate solar zenith angle
# according to http://solardat.uoregon.edu/SolarRadiationBasics.html
# calculations have been modified for positive East longitudes and time in UTC
# Extract time information
#( hour, minute, second, dummy, n ) = time.gmtime()[3:8]
# Calculate declination of the sun d
#if (is.vector(timein)){
	sza<-vector("numeric",length=length(timein))
	for(i in 1:length(timein)){
		time<-as.POSIXlt(timein[i])
		d<-23.45 * pi / 180 * sin(2 * pi * (284 + time$yday) / 365) # [rad]
#print (sprintf("d = %f", d * 180 / pi))
# Calculate equation of time
		if (time$yday <= 106){
			E_qt <- -14.2 * sin(pi * (time$yday + 7) / 111)      # Eq. SR.4a [minutes]
		}else{if (time$yday<= 166){
				E_qt <-   4.0 * sin(pi * (time$yday - 106) / 59)     # Eq. SR.4b [minutes]
			}else{if (time$yday<= 246){
					E_qt <-  -6.5 * sin(pi * (time$yday - 166) / 80)     # Eq. SR.4c [minutes]
				}else{E_qt <-  16.4 * sin(pi * (time$yday - 247) / 113)}}}    # Eq. SR.4d [minutes]
# Get UTC time T
		T<-time$hour + time$min / 60.0 + time$sec / 3600.0 # [hours]
# Calculate solar time T_solar (East longitudes are positive!)
		T_solar<-T + Lon / 15 + E_qt / 60 # [hours]
# Calculate hour angle w (positive: from midnight to noon, negative: from noon to midnight)
		w<-pi * (12 - T_solar) / 12 # [rad]
# Calculate solar zenith angle Z
		l<-Lat * pi / 180 # [rad]
		
		sza[i]<-acos(sin(l) * sin(d) + cos(l) * cos(d) * cos(w)) * 180 / pi # [deg]
		
	}
#}
	return(sza)
}

