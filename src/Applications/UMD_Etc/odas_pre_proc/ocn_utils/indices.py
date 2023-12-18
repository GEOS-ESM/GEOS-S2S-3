#! /usr/bin/env python
#

import numpy as np
import matplotlib.pyplot as plt
from numpy import *

def index(ind):
	
	class variable(object):
		def _init_(self, name=None):
			self.name = name
		

	####################################################
        # For TAO_ARGO experiments 
	tao=variable()
	tao.name      = 'tao'
	tao.long_name = 'TAO 10S-10N'
	tao.lat       = [-10, 10]
	tao.lon       = [150, 270]
	tao.rot       = 0

	taon=variable()
	taon.name      = 'taon'
	taon.long_name = 'TAO EQ-10N'
	taon.lat       = [0, 10]
	taon.lon       = [150, 270]
	taon.rot       = 0

	taos=variable()
	taos.name      = 'taos'
	taos.long_name = 'TAO 10S-EQ'
	taos.lat       = [-10, 0]
	taos.lon       = [150, 270]
	taos.rot       = 0

	taoe=variable()
	taoe.name      = 'taoe'
	taoe.long_name = 'Eastern TAO 10S-10N'
	taoe.lat       = [-10, 10]
	taoe.lon       = [230, 270]
	taoe.rot       = 0
	
	taoc=variable()
	taoc.name      = 'taoc'
	taoc.long_name = 'Central TAO 10S-10N'
	taoc.lat       = [-10, 10]
	taoc.lon       = [190, 230]
	taoc.rot       = 0
	
	taow=variable()
	taow.name      = 'taow'
	taow.long_name = 'Western TAO 10S-10N'
	taow.lat       = [-10, 10]
	taow.lon       = [150, 190]
	taow.rot       = 0

	taoen=variable()
	taoen.name      = 'taoen'
	taoen.long_name = 'Eastern TAO EQ-10N'
	taoen.lat       = [0, 10]
	taoen.lon       = [230, 270]
	taoen.rot       = 0
	
	taocn=variable()
	taocn.name      = 'taocn'
	taocn.long_name = 'Central TAO EQ-10N'
	taocn.lat       = [0, 10]
	taocn.lon       = [190, 230]
	taocn.rot       = 0
	
	taown=variable()
	taown.name      = 'taown'
	taown.long_name = 'Western TAO EQ-10N'
	taown.lat       = [0, 10]
	taown.lon       = [150, 190]
	taown.rot       = 0

	taoes=variable()
	taoes.name      = 'taoes'
	taoes.long_name = 'Eastern TAO 10S-EQ'
	taoes.lat       = [-10, 0]
	taoes.lon       = [230, 270]
	taoes.rot       = 0
	
	taocs=variable()
	taocs.name      = 'taocs'
	taocs.long_name = 'Central TAO 10S-EQ'
	taocs.lat       = [-10, 0]
	taocs.lon       = [190, 230]
	taocs.rot       = 0
	
	taows=variable()
	taows.name      = 'taows'
	taows.long_name = 'Western TAO 10S-EQ'
	taows.lat       = [-10, 0]
	taows.lon       = [150, 190]
	taows.rot       = 0
	####################################################


	glb=variable()
	glb.name      = 'glb'
	glb.long_name = 'Global'
	glb.lat       = [-75, 75]
	glb.lon       = [0, 360]
	glb.rot       = 0
	
	g30=variable()
	g30.name      = 'g30'
	g30.long_name = 'Global 30S-30N'
	g30.lat       = [-30, 30]
	g30.lon       = [0, 360]
	g30.rot       = 0
	
	gn=variable()
	gn.name      = 'gn'
	gn.long_name = 'Global 30N-90N'
	gn.lat       = [30, 90]
	gn.lon       = [0, 360]
	gn.rot       = 0

	gs=variable()
	gs.name      = 'gs'
	gs.long_name = 'Global 90S-30S'
	gs.lat       = [-90, -30]
	gs.lon       = [0, 360]
	gs.rot       = 0

	p30=variable()
	p30.name      = 'p30'
	p30.long_name = 'Pacific 30S-30N'
	p30.lat       = [-30, 30]
	p30.lon       = [120, 270]
	p30.rot       = 0
	
	p10=variable()
	p10.name      = 'p10'
	p10.long_name = 'Pacific 10S-10N'
	p10.lat       = [-10, 10]
	p10.lon       = [120, 270]
	p10.rot       = 0

	p5=variable()
	p5.name      = 'p5'
	p5.long_name = 'Pacific 5S-5N'
	p5.lat       = [-5, 5]
	p5.lon       = [120, 270]
	p5.rot       = 0

	a30=variable()
	a30.name      = 'a30'
	a30.long_name = 'Altantic 30S-30N'
	a30.lat       = [-30, 30]
	a30.lon       = [270, 30]
	a30.rot       = 1
	
	i30=variable()
	i30.name      = 'i30'
	i30.long_name = 'Indian 30S-30N'
	i30.lat       = [-30, 30]
	i30.lon       = [30, 120]
	i30.rot       = 0
	
	eqwpac=variable()
	eqwpac.name      = 'eqwpac'
	eqwpac.long_name = 'Equatorial West Pacific'
	eqwpac.lat       = [-5, 5]
	eqwpac.lon       = [130, 190]
	eqwpac.rot       = 0
	
	eqpac=variable()
	eqpac.name      = 'eqpac'
	eqpac.long_name = 'Equatorial Pacific'
	eqpac.lat       = [-5, 5]
	eqpac.lon       = [130, 270]
	eqpac.rot       = 0
	
	eqpac10=variable()
	eqpac10.name      = 'eqpac10'
	eqpac10.long_name = 'Equatorial Pacific 10S-10N'
	eqpac10.lat       = [-10, 10]
	eqpac10.lon       = [135, 270]
	eqpac10.rot       = 0

	nino12=variable()
	nino12.name      = 'nino12'
	nino12.long_name = 'Nino 12'
	nino12.lat       = [-10, 0]
	nino12.lon       = [270, 280]
	nino12.rot       = 0
	
	nino3=variable()
	nino3.name      = 'nino3'
	nino3.long_name = 'Nino 3'
	nino3.lat       = [-5, 5]
	nino3.lon       = [210, 270]
	nino3.rot       = 0
	
	nino34=variable()
	nino34.name      = 'nino34'
	nino34.long_name = 'Nino 3.4'
	nino34.lat       = [-5, 5]
	nino34.lon       = [190, 240]
	nino34.rot       = 0
	
	nino4=variable()
	nino4.name      = 'nino4'
	nino4.long_name = 'Nino 4'
	nino4.lat       = [-5, 5]
	nino4.lon       = [160, 210]
	nino4.rot       = 0
	
	trpac=variable()
	trpac.name      = 'trpac'
	trpac.long_name = 'Tropical Pacific'
	trpac.lat       = [-30, 30]
	trpac.lon       = [125, 280]
	trpac.rot       = 0
		
	spac=variable()
	spac.name      = 'spac'
	spac.long_name = 'South Pacific'
	spac.lat       = [-70, -30]
	spac.lon       = [150, 290]
	spac.rot       = 0
	
	npac=variable()
	npac.name      = 'npac'
	npac.long_name = 'North Pacific'
	npac.lat       = [30, 70]
	npac.lon       = [100, 260]
	npac.rot       = 0
	
	nstrpac=variable()
	nstrpac.name      = 'nstrpac'
	nstrpac.long_name = 'North SubTropical Pacific'
	nstrpac.lat       = [10, 30]
	nstrpac.lon       = [105, 270]
	nstrpac.rot       = 0
	
	sstrpac=variable()
	sstrpac.name      = 'sstrpac'
	sstrpac.long_name = 'South SubTropical Pacific'
	sstrpac.lat       = [-30, -10]
	sstrpac.lon       = [105, 270]
	sstrpac.rot       = 0
	
	eq1=variable()
	eq1.name      = 'eq1'
	eq1.long_name = 'Equatorial 1'
	eq1.lat       = [-5, 5]
	eq1.lon       = [230, 270]
	eq1.rot       = 0
	
	eq2=variable()
	eq2.name      = 'eq2'
	eq2.long_name = 'Equatorial 2'
	eq2.lat       = [-5, 5]
	eq2.lon       = [190, 230]
	eq2.rot       = 0
	
	eq3=variable()
	eq3.name      = 'eq3'
	eq3.long_name = 'Equatorial 3'
	eq3.lat       = [-5, 5]
	eq3.lon       = [150, 190]
	eq3.rot       = 0
	
	####################################################
	eqatl=variable()
	eqatl.name      = 'eqatl'
	eqatl.long_name = 'Equatorial Atlantic'
	eqatl.lat       = [-5, 5]
	eqatl.lon       = [290, 30]
	eqatl.rot       = 1
	
	atlnino=variable()
	atlnino.name      = 'atlnino'
	atlnino.long_name = 'Atlantic Nino'
	atlnino.lat       = [0, 10]
	atlnino.lon       = [300, 340]
	atlnino.rot       = 0
	
	natl=variable()
	natl.name      = 'natl'
	natl.long_name = 'North Atlantic'
	natl.lat       = [30, 70]
	natl.lon       = [290, 15]
	natl.rot       = 1
	
	satl=variable()
	satl.name      = 'satl'
	satl.long_name = 'South Atlantic'
	satl.lat       = [-70, -30]
	satl.lon       = [290, 20]
	satl.rot       = 1
	
	tratl=variable()
	tratl.name      = 'tratl'
	tratl.long_name = 'Tropical Atlantic'
	tratl.lat       = [-20, 30]
	tratl.lon       = [280, 20]
	tratl.rot       = 1
	
	nstratl=variable()
	nstratl.name      = 'nstratl'
	nstratl.long_name = 'North SubTropical Atlantic'
	nstratl.lat       = [5, 28]
	nstratl.lon       = [280, 20]
	nstratl.rot       = 1
	
	sstratl=variable()
	sstratl.name      = 'sstratl'
	sstratl.long_name = 'South SubTropical Atlantic'
	sstratl.lat       = [-20, 5]
	sstratl.lon       = [300, 20]
	sstratl.rot       = 1
	
	neatl=variable()
	neatl.name      = 'neatl'
	neatl.long_name = 'NorthEast Atlantic'
	neatl.lat       = [30, 70]
	neatl.lon       = [320, 15]
	neatl.rot       = 1
	
	nwatl=variable()
	nwatl.name      = 'nwatl'
	nwatl.long_name = 'NorthWest Atlantic'
	nwatl.lat       = [30, 70]
	nwatl.lon       = [260, 320]
	nwatl.rot       = 0
	
	atl1=variable()
	atl1.name      = 'atl1'
	atl1.long_name = 'Atlantic 1'
	atl1.lat       = [0, 10]
	atl1.lon       = [315, 340]
	atl1.rot       = 0
	
	atl2=variable()
	atl2.name      = 'atl2'
	atl2.long_name = 'Atlantic 2'
	atl2.lat       = [-3, 3]
	atl2.lon       = [0, 10]
	atl2.rot       = 0
	
	atl3=variable()
	atl3.name      = 'atl3'
	atl3.long_name = 'Atlantic 3'
	atl3.lat       = [-3, 3]
	atl3.lon       = [340, 360]
	atl3.rot       = 0
	
	####################################################
	eqind=variable()
	eqind.name      = 'eqind'
	eqind.long_name = 'Equatorial Indian'
	eqind.lat       = [-5, 5]
	eqind.lon       = [40, 120]
	eqind.rot       = 0
	
	sind=variable()
	sind.name      = 'sind'
	sind.long_name = 'South Indian'
	sind.lat       = [-70, -30]
	sind.lon       = [20, 150]
	sind.rot       = 0
	
	trind=variable()
	trind.name      = 'trind'
	trind.long_name = 'Tropical Indian'
	trind.lat       = [-30, 30]
	trind.lon       = [40, 120]
	trind.rot       = 0
	
	nstrind=variable()
	nstrind.name      = 'nstrind'
	nstrind.long_name = 'North SubTropical Indian'
	nstrind.lat       = [0, 10]
	nstrind.lon       = [50, 100]
	nstrind.rot       = 0
	
	sstrind=variable()
	sstrind.name      = 'sstrind'
	sstrind.long_name = 'South SubTropical Indian'
	sstrind.lat       = [-30, -10]
	sstrind.lon       = [50, 110]
	sstrind.rot       = 0
	
	ind1=variable()
	ind1.name      = 'ind1'
	ind1.long_name = 'Western Indian (Ind1)'
	ind1.lat       = [-10, 10]
	ind1.lon       = [50, 70]
	ind1.rot       = 0
	
	ind3=variable()
	ind3.name      = 'ind3'
	ind3.long_name = 'Central Indian (Ind3)'
	ind3.lat       = [-10, 0]
	ind3.lon       = [50, 90]
	ind3.rot       = 0
	
	ind2=variable()
	ind2.name      = 'ind2'
	ind2.long_name = 'Eastern Indian (Ind2)'
	ind2.lat       = [-10, 0]
	ind2.lon       = [90, 110]
	ind2.rot       = 0
	
	for variable in [eval(ind)]:
		index.name      = variable.name
		index.long_name = variable.long_name
		index.lat       = variable.lat
		index.lon       = variable.lon
		index.rot       = variable.rot
	
	return index

