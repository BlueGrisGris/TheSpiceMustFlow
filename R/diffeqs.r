### Define diff eq's 
### EQN 1 ###
### Temp free tropo
### TODO: figure out convection
dTUdt <- function(CONDEN, CONVEC, TT, TL, TU){
	dTUdt <- (
		Le*CONDEN
		+ rhoL*cp*hL*(CONVEC)
		+ (1-emissL)*(emissU + emissW*(W/W0))*emissS*sig*TT^4
		+ (emissU + emissW*(W/W0))*emissL*sig*TL^4
		- 2*(emissU + emissW*(W/W0))*sig*TU^4
		)/
	(rhoU*hU*cp)
	return(dTUdt ) 
}#end func TU

### EQN 2 ###
### Temp boundary layer
### TODO: convection term
dTLdt <- function(CONVEC, TT, TL, TU){
	return(
	(Qs 
	- rhoL*cp*hL*(CONVEC) 
	+ emissL * emissS * sig * TT^4
	+ (emissU + emissW*(W/W0)) emissL * sig * TU^4
	- 2*emissL*sig*TL^4
	)/
	(rhoL*hL*cp)
)#ned return
}#emd func dTLdt	

#### EQN 3 ###
### humdidity free tropo
### TODO: convection term
dQUdt <- function(CONVEC, CONDEN){
	return(
	(
	rhoL*hL*(CONVEC) - CONDEN
	)/
	(rhoU*hU)
)#end return
}#end func QUdt 

### EQN 4 ###
### humdidity boundary layer
dQLdt <- function(ST,SD,QL,B,CONVEC){
	return(
	       (#start top frac
		((1-b) + S*b) * E(ST,QL)
		+ B * R(SD, QL)
		- rhoL*hL*CONVEC
		)/#end top of fraction
	       (rhoL*hL)
	)#edn return
}#end fun dqldt

### EQN 5 ###
### Liquid H2O in free tropo
dWdt <- function(CONDEN, PRECIP){
	return(
	       CONDEN - PRECIP
	       )#end return
}#end func dwdt

### EQN 6 ###
### Condensation 
CONDEN <- function(QU,TU){
	### TODO: what rho is this ??
	### TODO: add a qsat
	### TODO: are we sure this is rhoU bc the paper just has rho lol
	return(
	 rhoU*hU*(QU-qSat(TU))
	 )#end return
}#end func TU

### EQN 7 ###
### surface layer temp
dTTdt <- function(B,W,TT,TD,TU,ST,SD,QL){
	return(
	       (#start frac top
	       (1-(B*alphaV+(1-b)*alphaE))*Frad
	       - Qs
	       - emissS*sig*TT^4
	       + emissL*sig*TL^4
	       + (1-emissL)*emissS*(emissU+emissW*(W/W0))*sig*TU^4
	       - Le*E(ST,QL)*((1-B)+S*B)
	       - Le*B*R(SD,QL)
	       - rhoS*cps*(ZT+ZD)((TT-TD)/tauT)
	       )/#end frac top
	       (rhoS*cps*ZT)
	       )#end ret
}#end dTTdt

### EQN 8 ###
### Deep soil layer temp

dTDdt <- function(TT,TD){
	return(
	       (#start top frac
		-(ZT+ZD)*((TD-TT)/tauT)
		-(ZD+Z0)*((TD-T0)/tauD)

		)#end top frac

	       )




}#dend dTDdt

### Clausius Clayperon
qSat <- function(TU){

	
	return(qSat)
}#end func qSat

