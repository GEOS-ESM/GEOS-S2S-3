!=======================================================================
!
! $Id: setkin_lchem.h,v 1.1.1.1.22.1.46.1.20.1.144.1.8.1 2016/12/21 18:59:05 mmanyin Exp $
!
! FILE
!   setkin_lchem.h - character labels for species and reactions
!             (setkin_lchem.h)
!   12 JUN 02 - PSC
!
! DESCRIPTION
!   Include file that provides ascii strings identifying reactions
!   and species
!
!  Chemistry input file:    09/2016
!  Reaction dictionary:     GMI_Combo_rxns_124species_SO2_JPL15.db
!  Setkin files generated:  Wed Sep  7 15:23:58 2016
!
!=======================================================================


      integer i

      character*16 lchemvar(NSP), ldynvar(NDYN)
      character*80 lqkchem(NUM_K),  lqjchem(NUM_J)
!
!.... All species labels
!
      data lchemvar(1) /"CH2O"/
      data lchemvar(2) /"CH4"/
      data lchemvar(3) /"CO"/
      data lchemvar(4) /"H"/
      data lchemvar(5) /"H2"/
      data lchemvar(6) /"HCOOH"/
      data lchemvar(7) /"HNO2"/
      data lchemvar(8) /"HNO3"/
      data lchemvar(9) /"HNO4"/
      data lchemvar(10) /"H2O"/
      data lchemvar(11) /"HO2"/
      data lchemvar(12) /"H2O2"/
      data lchemvar(13) /"MO2"/
      data lchemvar(14) /"MOH"/
      data lchemvar(15) /"MP"/
      data lchemvar(16) /"N"/
      data lchemvar(17) /"N2O"/
      data lchemvar(18) /"NO"/
      data lchemvar(19) /"NO2"/
      data lchemvar(20) /"NO3"/
      data lchemvar(21) /"N2O5"/
      data lchemvar(22) /"O"/
      data lchemvar(23) /"O1D"/
      data lchemvar(24) /"O3"/
      data lchemvar(25) /"OH"/
      data lchemvar(26) /"Br"/
      data lchemvar(27) /"BrCl"/
      data lchemvar(28) /"BrO"/
      data lchemvar(29) /"BrONO2"/
      data lchemvar(30) /"HBr"/
      data lchemvar(31) /"HOBr"/
      data lchemvar(32) /"Cl"/
      data lchemvar(33) /"Cl2"/
      data lchemvar(34) /"ClO"/
      data lchemvar(35) /"Cl2O2"/
      data lchemvar(36) /"ClONO2"/
      data lchemvar(37) /"HCl"/
      data lchemvar(38) /"HOCl"/
      data lchemvar(39) /"OClO"/
      data lchemvar(40) /"CH3Br"/
      data lchemvar(41) /"CH3Cl"/
      data lchemvar(42) /"CH3CCl3"/
      data lchemvar(43) /"CCl4"/
      data lchemvar(44) /"CFCl3"/
      data lchemvar(45) /"CF2Cl2"/
      data lchemvar(46) /"CFC113"/
      data lchemvar(47) /"CFC114"/
      data lchemvar(48) /"CFC115"/
      data lchemvar(49) /"HCFC22"/
      data lchemvar(50) /"HCFC141b"/
      data lchemvar(51) /"HCFC142b"/
      data lchemvar(52) /"CF2Br2"/
      data lchemvar(53) /"CF2ClBr"/
      data lchemvar(54) /"CF3Br"/
      data lchemvar(55) /"H2402"/
      data lchemvar(56) /"A3O2"/
      data lchemvar(57) /"ACTA"/
      data lchemvar(58) /"ALD2"/
      data lchemvar(59) /"ALK4"/
      data lchemvar(60) /"ATO2"/
      data lchemvar(61) /"B3O2"/
      data lchemvar(62) /"C2H6"/
      data lchemvar(63) /"C3H8"/
      data lchemvar(64) /"EOH"/
      data lchemvar(65) /"ETO2"/
      data lchemvar(66) /"ETP"/
      data lchemvar(67) /"GCO3"/
      data lchemvar(68) /"GLYC"/
      data lchemvar(69) /"GLYX"/
      data lchemvar(70) /"GP"/
      data lchemvar(71) /"GPAN"/
      data lchemvar(72) /"HAC"/
      data lchemvar(73) /"IALD"/
      data lchemvar(74) /"IAO2"/
      data lchemvar(75) /"IAP"/
      data lchemvar(76) /"INO2"/
      data lchemvar(77) /"INPN"/
      data lchemvar(78) /"ISN1"/
      data lchemvar(79) /"ISNP"/
      data lchemvar(80) /"ISOP"/
      data lchemvar(81) /"KO2"/
      data lchemvar(82) /"MACR"/
      data lchemvar(83) /"MAN2"/
      data lchemvar(84) /"MAO3"/
      data lchemvar(85) /"MAOP"/
      data lchemvar(86) /"MAP"/
      data lchemvar(87) /"MCO3"/
      data lchemvar(88) /"MEK"/
      data lchemvar(89) /"MGLY"/
      data lchemvar(90) /"MRO2"/
      data lchemvar(91) /"MRP"/
      data lchemvar(92) /"MVK"/
      data lchemvar(93) /"MVN2"/
      data lchemvar(94) /"PAN"/
      data lchemvar(95) /"PMN"/
      data lchemvar(96) /"PO2"/
      data lchemvar(97) /"PP"/
      data lchemvar(98) /"PPN"/
      data lchemvar(99) /"PRN1"/
      data lchemvar(100) /"PRPE"/
      data lchemvar(101) /"PRPN"/
      data lchemvar(102) /"R4N1"/
      data lchemvar(103) /"R4N2"/
      data lchemvar(104) /"R4O2"/
      data lchemvar(105) /"R4P"/
      data lchemvar(106) /"RA3P"/
      data lchemvar(107) /"RB3P"/
      data lchemvar(108) /"RCHO"/
      data lchemvar(109) /"RCO3"/
      data lchemvar(110) /"RCOOH"/
      data lchemvar(111) /"RIO1"/
      data lchemvar(112) /"RIO2"/
      data lchemvar(113) /"RIP"/
      data lchemvar(114) /"ROH"/
      data lchemvar(115) /"RP"/
      data lchemvar(116) /"VRO2"/
      data lchemvar(117) /"VRP"/
      data lchemvar(118) /"ACET"/
      data lchemvar(119) /"N2"/
      data lchemvar(120) /"O2"/
      data lchemvar(121) /"NUMDENS"/
      data lchemvar(122) /"HNO3COND"/
!
!.... Dynamic (transported) species labels
!
      data ldynvar(1) /"CH2O"/
      data ldynvar(2) /"CH4"/
      data ldynvar(3) /"CO"/
      data ldynvar(4) /"H"/
      data ldynvar(5) /"H2"/
      data ldynvar(6) /"HCOOH"/
      data ldynvar(7) /"HNO2"/
      data ldynvar(8) /"HNO3"/
      data ldynvar(9) /"HNO4"/
      data ldynvar(10) /"H2O"/
      data ldynvar(11) /"HO2"/
      data ldynvar(12) /"H2O2"/
      data ldynvar(13) /"MO2"/
      data ldynvar(14) /"MOH"/
      data ldynvar(15) /"MP"/
      data ldynvar(16) /"N"/
      data ldynvar(17) /"N2O"/
      data ldynvar(18) /"NO"/
      data ldynvar(19) /"NO2"/
      data ldynvar(20) /"NO3"/
      data ldynvar(21) /"N2O5"/
      data ldynvar(22) /"O"/
      data ldynvar(23) /"O1D"/
      data ldynvar(24) /"O3"/
      data ldynvar(25) /"OH"/
      data ldynvar(26) /"Br"/
      data ldynvar(27) /"BrCl"/
      data ldynvar(28) /"BrO"/
      data ldynvar(29) /"BrONO2"/
      data ldynvar(30) /"HBr"/
      data ldynvar(31) /"HOBr"/
      data ldynvar(32) /"Cl"/
      data ldynvar(33) /"Cl2"/
      data ldynvar(34) /"ClO"/
      data ldynvar(35) /"Cl2O2"/
      data ldynvar(36) /"ClONO2"/
      data ldynvar(37) /"HCl"/
      data ldynvar(38) /"HOCl"/
      data ldynvar(39) /"OClO"/
      data ldynvar(40) /"CH3Br"/
      data ldynvar(41) /"CH3Cl"/
      data ldynvar(42) /"CH3CCl3"/
      data ldynvar(43) /"CCl4"/
      data ldynvar(44) /"CFCl3"/
      data ldynvar(45) /"CF2Cl2"/
      data ldynvar(46) /"CFC113"/
      data ldynvar(47) /"CFC114"/
      data ldynvar(48) /"CFC115"/
      data ldynvar(49) /"HCFC22"/
      data ldynvar(50) /"HCFC141b"/
      data ldynvar(51) /"HCFC142b"/
      data ldynvar(52) /"CF2Br2"/
      data ldynvar(53) /"CF2ClBr"/
      data ldynvar(54) /"CF3Br"/
      data ldynvar(55) /"H2402"/
      data ldynvar(56) /"A3O2"/
      data ldynvar(57) /"ACTA"/
      data ldynvar(58) /"ALD2"/
      data ldynvar(59) /"ALK4"/
      data ldynvar(60) /"ATO2"/
      data ldynvar(61) /"B3O2"/
      data ldynvar(62) /"C2H6"/
      data ldynvar(63) /"C3H8"/
      data ldynvar(64) /"EOH"/
      data ldynvar(65) /"ETO2"/
      data ldynvar(66) /"ETP"/
      data ldynvar(67) /"GCO3"/
      data ldynvar(68) /"GLYC"/
      data ldynvar(69) /"GLYX"/
      data ldynvar(70) /"GP"/
      data ldynvar(71) /"GPAN"/
      data ldynvar(72) /"HAC"/
      data ldynvar(73) /"IALD"/
      data ldynvar(74) /"IAO2"/
      data ldynvar(75) /"IAP"/
      data ldynvar(76) /"INO2"/
      data ldynvar(77) /"INPN"/
      data ldynvar(78) /"ISN1"/
      data ldynvar(79) /"ISNP"/
      data ldynvar(80) /"ISOP"/
      data ldynvar(81) /"KO2"/
      data ldynvar(82) /"MACR"/
      data ldynvar(83) /"MAN2"/
      data ldynvar(84) /"MAO3"/
      data ldynvar(85) /"MAOP"/
      data ldynvar(86) /"MAP"/
      data ldynvar(87) /"MCO3"/
      data ldynvar(88) /"MEK"/
      data ldynvar(89) /"MGLY"/
      data ldynvar(90) /"MRO2"/
      data ldynvar(91) /"MRP"/
      data ldynvar(92) /"MVK"/
      data ldynvar(93) /"MVN2"/
      data ldynvar(94) /"PAN"/
      data ldynvar(95) /"PMN"/
      data ldynvar(96) /"PO2"/
      data ldynvar(97) /"PP"/
      data ldynvar(98) /"PPN"/
      data ldynvar(99) /"PRN1"/
      data ldynvar(100) /"PRPE"/
      data ldynvar(101) /"PRPN"/
      data ldynvar(102) /"R4N1"/
      data ldynvar(103) /"R4N2"/
      data ldynvar(104) /"R4O2"/
      data ldynvar(105) /"R4P"/
      data ldynvar(106) /"RA3P"/
      data ldynvar(107) /"RB3P"/
      data ldynvar(108) /"RCHO"/
      data ldynvar(109) /"RCO3"/
      data ldynvar(110) /"RCOOH"/
      data ldynvar(111) /"RIO1"/
      data ldynvar(112) /"RIO2"/
      data ldynvar(113) /"RIP"/
      data ldynvar(114) /"ROH"/
      data ldynvar(115) /"RP"/
      data ldynvar(116) /"VRO2"/
      data ldynvar(117) /"VRP"/
      data ldynvar(118) /"HNO3COND"/
!
!.... Thermal reaction labels
!
      data (lqkchem(i), i=1,10) / &
     & 'O + O2 = O3', &
     & 'O + O3 = 2 O2', &
     & 'N2 + O1D = N2 + O', &
     & 'O1D + O2 = O + O2', &
     & 'O1D + O3 = 2 O2', &
     & 'O1D + O3 = 2 O + O2', &
     & 'H2O + O1D = 2 OH', &
     & 'H2 + O1D = H + OH', &
     & 'N2O + O1D = N2 + O2', &
     & 'N2O + O1D = 2 NO' /

      data (lqkchem(i), i=11,20) / &
     & 'CH4 + O1D = MO2 + OH', &
     & 'CH4 + O1D = CH2O + H + HO2', &
     & 'CH4 + O1D = CH2O + H2', &
     & 'CF2Cl2 + O1D = 2 Cl', &
     & 'CFC113 + O1D = 3 Cl', &
     & 'CFC114 + O1D = 2 Cl', &
     & 'CFC115 + O1D = Cl', &
     & 'HCFC22 + O1D = Cl', &
     & 'HCFC141b + O1D = 2 Cl', &
     & 'HCFC142b + O1D = Cl' /

      data (lqkchem(i), i=21,30) / &
     & 'H + O2 = HO2', &
     & 'H + O3 = O2 + OH', &
     & 'O + OH = H + O2', &
     & 'HO2 + O = O2 + OH', &
     & 'H + HO2 = 2 OH', &
     & 'NO + O3 = NO2 + O2', &
     & 'O3 + OH = HO2 + O2', &
     & 'HO2 + O3 = 2 O2 + OH', &
     & 'NO2 + O3 = NO3 + O2', &
     & 'OH + OH = H2O + O' /

      data (lqkchem(i), i=31,40) / &
     & 'OH + OH = H2O2', &
     & 'HO2 + OH = H2O + O2', &
     & 'H2O2 + OH = H2O + HO2', &
     & 'HO2 + NO = NO2 + OH', &
     & 'HO2 + HO2 = H2O2 + O2', &
     & 'H2O + HO2 + HO2 = H2O + H2O2 + O2', &
     & 'H2 + OH = H + H2O', &
     & 'CO + OH = H', &
     & 'CH4 + OH = H2O + MO2', &
     & 'MO2 + NO = CH2O + HO2 + NO2' /

      data (lqkchem(i), i=41,50) / &
     & 'HO2 + MO2 = MP + O2', &
     & 'MO2 + MO2 = CH2O + MOH + O2', &
     & 'MO2 + MO2 = 2 CH2O + 2 HO2', &
     & 'MP + OH = H2O + MO2', &
     & 'MP + OH = CH2O + H2O + OH', &
     & 'CH2O + OH = CO + H2O + HO2', &
     & 'N + O2 = NO + O', &
     & 'N + NO = N2 + O', &
     & 'NO2 + O = NO + O2', &
     & 'NO3 + O = NO2 + O2' /

      data (lqkchem(i), i=51,60) / &
     & 'NO2 + OH = HNO3', &
     & 'HNO3 + OH = H2O + NO3', &
     & 'NO + OH = HNO2', &
     & 'HNO2 + OH = H2O + NO2', &
     & 'HO2 + NO2 = HNO4', &
     & 'HNO4 = HO2 + NO2', &
     & 'HNO4 + OH = H2O + NO2 + O2', &
     & 'HO2 + NO3 = NO2 + O2 + OH', &
     & 'NO + NO3 = 2 NO2', &
     & 'NO3 + OH = HO2 + NO2' /

      data (lqkchem(i), i=61,70) / &
     & 'NO2 + NO3 = N2O5', &
     & 'N2O5 = NO2 + NO3', &
     & 'HCOOH + OH = H2O + HO2', &
     & 'MOH + OH = CH2O + HO2', &
     & 'NO2 + NO3 = NO + NO2 + O2', &
     & 'CH2O + NO3 = CO + HNO3 + HO2', &
     & 'Cl + O3 = ClO + O2', &
     & 'Cl + H2 = H + HCl', &
     & 'Cl + H2O2 = HCl + HO2', &
     & 'Cl + HO2 = HCl + O2' /

      data (lqkchem(i), i=71,80) / &
     & 'Cl + HO2 = ClO + OH', &
     & 'ClO + O = Cl + O2', &
     & 'ClO + OH = Cl + HO2', &
     & 'ClO + OH = HCl + O2', &
     & 'ClO + HO2 = HOCl + O2', &
     & 'ClO + HO2 = HCl + O3', &
     & 'ClO + NO = Cl + NO2', &
     & 'ClO + NO2 = ClONO2', &
     & 'ClO + ClO = 2 Cl + O2', &
     & 'ClO + ClO = Cl2 + O2' /

      data (lqkchem(i), i=81,90) / &
     & 'ClO + ClO = Cl + OClO', &
     & 'ClO + ClO = Cl2O2', &
     & 'Cl2O2 = 2 ClO', &
     & 'HCl + OH = Cl + H2O', &
     & 'HOCl + OH = ClO + H2O', &
     & 'ClONO2 + O = ClO + NO3', &
     & 'ClONO2 + OH = HOCl + NO3', &
     & 'Cl + ClONO2 = Cl2 + NO3', &
     & 'Br + O3 = BrO + O2', &
     & 'Br + HO2 = HBr + O2' /

      data (lqkchem(i), i=91,100) / &
     & 'Br + CH2O = CO + HBr + HO2', &
     & 'BrO + O = Br + O2', &
     & 'BrO + HO2 = HOBr + O2', &
     & 'BrO + NO = Br + NO2', &
     & 'BrO + NO2 = BrONO2', &
     & 'BrO + ClO = Br + OClO', &
     & 'BrO + ClO = Br + Cl + O2', &
     & 'BrO + ClO = BrCl + O2', &
     & 'BrO + BrO = 2 Br + O2', &
     & 'HBr + OH = Br + H2O' /

      data (lqkchem(i), i=101,110) / &
     & 'CH2O + O = CO + HO2 + OH', &
     & 'CH4 + Cl = HCl + MO2', &
     & 'CH2O + Cl = CO + HCl + HO2', &
     & 'CH3Cl + OH = Cl + H2O + HO2', &
     & 'CH3CCl3 + OH = 3 Cl + H2O', &
     & 'HCFC22 + OH = Cl + H2O', &
     & 'HCFC141b + OH = 2 Cl + H2O', &
     & 'HCFC142b + OH = Cl + H2O', &
     & 'CH3Cl + Cl = CO + 2 HCl + HO2', &
     & 'CH3Br + OH = Br + H2O + HO2' /

      data (lqkchem(i), i=111,120) / &
     & 'ALD2 + OH = H2O + MCO3', &
     & 'ALD2 + NO3 = HNO3 + MCO3', &
     & 'MCO3 + NO2 = PAN', &
     & 'PAN = MCO3 + NO2', &
     & 'MCO3 + NO = MO2 + NO2', &
     & 'C2H6 + OH = ETO2 + H2O', &
     & 'ETO2 + NO = ALD2 + HO2 + NO2', &
     & 'C3H8 + OH = B3O2', &
     & 'C3H8 + OH = A3O2', &
     & 'A3O2 + NO = HO2 + NO2 + RCHO' /

      data (lqkchem(i), i=121,130) / &
     & 'NO + PO2 = ALD2 + CH2O + HO2 + NO2', &
     & 'ALK4 + OH = R4O2', &
     & 'NO + R4O2 =  0.05 A3O2 +  0.32 ACET +  0.32 ALD2 +  0.18 B3O', &
     & 'NO + R4O2 = R4N2', &
     & 'NO + R4N1 =  0.75 ALD2 +  0.39 CH2O + 2 NO2 +  0.30 R4O2 +  ', &
     & 'ATO2 + NO =  0.19 CH2O +  0.77 HO2 +  0.19 MCO3 +  0.77 MGLY', &
     & 'KO2 + NO =  0.93 ALD2 +  0.93 MCO3 +  0.93 NO2 +  0.07 R4N2', &
     & 'NO + RIO2 =  0.69 CH2O +  0.86 HO2 +  0.13 IALD +  0.29 MACR', &
     & 'NO + RIO2 = HNO3', &
     & 'NO + RIO1 =  0.75 CH2O + HO2 + IALD + NO2' /

      data (lqkchem(i), i=131,140) / &
     & 'NO + RIO1 = HNO3', &
     & 'IAO2 + NO =  0.35 CH2O +  0.27 CO +  0.24 GLYC +  0.17 GLYX ', &
     & 'NO + VRO2 =  0.28 CH2O +  0.72 GLYC +  0.28 HO2 +  0.72 MCO3', &
     & 'NO + VRO2 = HNO3', &
     & 'MRO2 + NO =  0.17 CH2O +  0.83 CO +  0.83 HAC + HO2 +  0.17 ', &
     & 'MRO2 + NO = HNO3', &
     & 'MVN2 + NO =  0.30 CH2O +  0.60 GLYC +  0.10 HNO3 +  0.30 HO2', &
     & 'MAN2 + NO = CH2O + MGLY + 2 NO2', &
     & 'B3O2 + NO = ACET + HO2 + NO2', &
     & 'INO2 + NO =  0.15 CH2O +  0.85 HNO3 +  0.80 HO2 +  0.10 MACR' /

      data (lqkchem(i), i=141,150) / &
     & 'NO + PRN1 = ALD2 + CH2O + 2 NO2', &
     & 'ALK4 + NO3 = HNO3 + R4O2', &
     & 'OH + R4N2 = H2O + R4N1', &
     & 'ACTA + OH = H2O + MO2', &
     & 'OH + RCHO = H2O + RCO3', &
     & 'NO2 + RCO3 = PPN', &
     & 'GCO3 + NO2 = GPAN', &
     & 'MAO3 + NO2 = PMN', &
     & 'PPN = NO2 + RCO3', &
     & 'GPAN = GCO3 + NO2' /

      data (lqkchem(i), i=151,160) / &
     & 'NO + RCO3 = ETO2 + NO2', &
     & 'GCO3 + NO = CH2O + HO2 + NO2', &
     & 'MAO3 + NO = 4 CH2O + HO2 + NO2', &
     & 'NO3 + RCHO = HNO3 + RCO3', &
     & 'ACET + OH = ATO2 + H2O', &
     & 'A3O2 + MO2 =  0.75 CH2O + HO2 +  0.25 MOH +  0.75 RCHO +  0.', &
     & 'MO2 + PO2 =  0.50 ALD2 +  1.25 CH2O +  0.16 HAC + HO2 +  0.2', &
     & 'HO2 + R4O2 = R4P', &
     & 'HO2 + R4N1 = R4N2', &
     & 'ATO2 + HO2 = MCO3 + MO2' /

      data (lqkchem(i), i=161,170) / &
     & 'HO2 + KO2 = MGLY + MO2', &
     & 'HO2 + RIO2 = RIP', &
     & 'HO2 + RIO1 = RIP', &
     & 'HO2 + IAO2 = IAP', &
     & 'HO2 + ISN1 = ISNP', &
     & 'HO2 + VRO2 = VRP', &
     & 'HO2 + MRO2 = MRP', &
     & 'HO2 + MVN2 = ISNP', &
     & 'HO2 + MAN2 = ISNP', &
     & 'B3O2 + HO2 = RB3P' /

      data (lqkchem(i), i=171,180) / &
     & 'HO2 + INO2 = INPN', &
     & 'HO2 + PRN1 = PRPN', &
     & 'MEK + OH = H2O + KO2', &
     & 'ETO2 + MO2 =  0.75 ALD2 +  0.75 CH2O +  0.25 EOH + HO2 +  0.', &
     & 'MEK + NO3 = HNO3 + KO2', &
     & 'MO2 + R4O2 =  0.03 A3O2 +  0.16 ACET +  0.16 ALD2 +  0.09 B3', &
     & 'MO2 + R4N1 =  0.38 ALD2 +  0.95 CH2O +  0.50 HO2 +  0.25 MOH', &
     & 'ATO2 + MO2 =  0.85 CH2O +  0.90 HO2 +  0.10 MCO3 +  0.25 MEK', &
     & 'KO2 + MO2 =  0.50 ALD2 +  0.75 CH2O +  0.50 HO2 +  0.50 MCO3', &
     & 'MO2 + RIO2 =  1.10 CH2O +  0.93 HO2 +  0.06 IALD +  0.14 MAC' /

      data (lqkchem(i), i=181,190) / &
     & 'MO2 + RIO1 =  1.13 CH2O + HO2 +  0.50 IALD +  0.25 MEK +  0.', &
     & 'IAO2 + MO2 =  0.95 CH2O +  0.15 CO +  0.13 GLYC +  0.09 GLYX', &
     & 'ISN1 + MO2 =  0.75 CH2O +  0.50 GLYC +  0.50 HAC +  0.50 HO2', &
     & 'MO2 + VRO2 =  0.89 CH2O +  0.36 GLYC +  0.64 HO2 +  0.36 MCO', &
     & 'MO2 + MRO2 =  0.84 CH2O +  0.42 CO +  0.42 HAC + HO2 +  0.25', &
     & 'MO2 + MVN2 =  1.25 CH2O +  0.75 HO2 +  0.25 MCO3 +  0.25 MGL', &
     & 'MAN2 + MO2 =  1.25 CH2O +  0.50 HO2 +  0.50 MGLY +  0.25 MOH', &
     & 'B3O2 + MO2 =  0.75 ACET +  0.75 CH2O + HO2 +  0.25 MOH +  0.', &
     & 'INO2 + MO2 =  0.83 CH2O +  0.43 HNO3 +  0.90 HO2 +  0.05 MAC', &
     & 'MO2 + PRN1 =  0.50 ALD2 +  1.25 CH2O +  0.50 HO2 +  0.25 MOH' /

      data (lqkchem(i), i=191,200) / &
     & 'EOH + OH = ALD2 + HO2', &
     & 'OH + ROH = HO2 + RCHO', &
     & 'ETO2 + ETO2 = 2 ALD2 + 2 HO2', &
     & 'ETO2 + ETO2 = ALD2 + EOH', &
     & 'ETO2 + HO2 = ETP', &
     & 'A3O2 + HO2 = RA3P', &
     & 'HO2 + PO2 = PP', &
     & 'HO2 + MCO3 = ACTA + O3', &
     & 'HO2 + MCO3 = MAP', &
     & 'HO2 + RCO3 =  0.30 O3 +  0.30 RCOOH +  0.70 RP' /

      data (lqkchem(i), i=201,210) / &
     & 'GCO3 + HO2 =  0.70 GP +  0.30 O3 +  0.30 RCOOH', &
     & 'HO2 + MAO3 =  0.70 MAOP +  0.30 O3 +  0.30 RCOOH', &
     & 'OH + PRPE = PO2', &
     & 'O3 + PRPE =  0.50 ALD2 +  0.54 CH2O +  0.42 CO +  0.06 H2 + ', &
     & 'PMN = MAO3 + NO2', &
     & 'OH + PMN =  2.23 CH2O +  0.59 HAC + 2 HO2 + NO2', &
     & 'O3 + PMN =  0.60 CH2O + HO2 + NO2', &
     & 'GLYC + OH =  0.80 GCO3 +  0.20 GLYX +  0.20 HO2', &
     & 'NO3 + PRPE = PRN1', &
     & 'GLYX + OH = 2 CO + HO2' /

      data (lqkchem(i), i=211,220) / &
     & 'MGLY + OH = CO + MCO3', &
     & 'GLYX + NO3 = 2 CO + HNO3 + HO2', &
     & 'MGLY + NO3 = CO + HNO3 + MCO3', &
     & 'ISOP + OH = RIO2', &
     & 'MVK + OH = VRO2', &
     & 'MACR + OH =  0.50 MAO3 +  0.50 MRO2', &
     & 'HAC + OH = HO2 + MGLY', &
     & 'A3O2 + MCO3 = HO2 + MO2 + RCHO', &
     & 'MCO3 + PO2 = ALD2 + CH2O + HO2 + MO2', &
     & 'A3O2 + MCO3 = ACTA + RCHO' /

      data (lqkchem(i), i=221,230) / &
     & 'MCO3 + PO2 = ACTA +  0.65 HAC +  0.35 RCHO', &
     & 'ISOP + O3 =  0.90 CH2O +  0.05 CO +  0.06 HO2 +  0.39 MACR +', &
     & 'MVK + O3 =  0.04 ALD2 +  0.80 CH2O +  0.05 CO +  0.06 HO2 + ', &
     & 'MACR + O3 =  0.70 CH2O +  0.20 CO +  0.28 HO2 +  0.80 MGLY +', &
     & 'ISOP + NO3 = INO2', &
     & 'MVK + NO3 = MVN2', &
     & 'MACR + NO3 = MAN2', &
     & 'MACR + NO3 = HNO3 + MAO3', &
     & 'MO2 + RCO3 = CH2O + ETO2 + HO2', &
     & 'GCO3 + MO2 = 2 CH2O + 2 HO2' /

      data (lqkchem(i), i=231,240) / &
     & 'MAO3 + MO2 = 2 CH2O + HO2 + MCO3', &
     & 'MO2 + RCO3 = CH2O + RCOOH', &
     & 'GCO3 + MO2 = CH2O + RCOOH', &
     & 'MAO3 + MO2 = CH2O + RCOOH', &
     & 'INPN + OH = INO2', &
     & 'OH + PRPN = PRN1', &
     & 'ETP + OH =  0.50 ALD2 +  0.50 ETO2 +  0.50 OH', &
     & 'OH + RA3P =  0.50 A3O2 +  0.50 OH +  0.50 RCHO', &
     & 'OH + RB3P =  0.50 B3O2 +  0.50 OH +  0.50 RCHO', &
     & 'OH + R4P =  0.50 OH +  0.50 R4O2 +  0.50 RCHO' /

      data (lqkchem(i), i=241,250) / &
     & 'OH + RP =  0.50 ALD2 +  0.50 OH +  0.50 RCO3', &
     & 'OH + PP =  0.50 OH +  0.50 PO2 +  0.50 RCHO', &
     & 'GP + OH =  0.50 CH2O +  0.50 GCO3 +  0.50 OH', &
     & 'OH + RIP =  0.50 IAO2 +  0.10 RIO1 +  0.40 RIO2', &
     & 'IAP + OH =  0.50 IAO2 +  0.50 OH +  0.50 RCHO', &
     & 'ISNP + OH =  0.50 ISN1 +  0.50 NO2 +  0.50 OH +  0.50 RCHO', &
     & 'OH + VRP =  0.50 OH +  0.50 RCHO +  0.50 VRO2', &
     & 'MRP + OH =  0.50 MRO2 +  0.50 OH +  0.50 RCHO', &
     & 'MAOP + OH =  0.50 MAO3 +  0.50 OH +  0.50 RCHO', &
     & 'MAP + OH =  0.50 CH2O +  0.50 MCO3 +  0.50 OH' /

      data (lqkchem(i), i=251,260) / &
     & 'C2H6 + NO3 = ETO2 + HNO3', &
     & 'IALD + OH =  0.15 HO2 +  0.44 IAO2 +  0.41 MAO3', &
     & 'IALD + O3 =  0.12 CH2O +  0.28 GLYC +  0.20 GLYX +  0.20 HAC', &
     & 'MCO3 + MCO3 = 2 MO2', &
     & 'MCO3 + MO2 = CH2O + HO2 + MO2', &
     & 'MCO3 + MO2 = ACTA + CH2O', &
     & 'MCO3 + R4O2 =  0.05 A3O2 +  0.32 ACET +  0.32 ALD2 +  0.18 B', &
     & 'ATO2 + MCO3 =  0.20 CH2O +  0.80 HO2 +  0.20 MCO3 +  0.80 MG', &
     & 'KO2 + MCO3 = ALD2 + MCO3 + MO2', &
     & 'MCO3 + RIO2 =  0.69 CH2O +  0.86 HO2 +  0.13 IALD +  0.29 MA' /

      data (lqkchem(i), i=261,270) / &
     & 'MCO3 + RIO1 =  0.75 CH2O + HO2 + IALD + MO2', &
     & 'IAO2 + MCO3 =  0.40 CH2O +  0.29 CO +  0.26 GLYC +  0.18 GLY', &
     & 'ISN1 + MCO3 = GLYC + HAC + MO2 + NO2', &
     & 'MCO3 + VRO2 =  0.28 CH2O +  0.72 GLYC +  0.28 HO2 +  0.72 MC', &
     & 'MCO3 + MRO2 =  0.17 CH2O +  0.83 CO +  0.83 HAC + HO2 +  0.1', &
     & 'B3O2 + MCO3 = ACET + HO2 + MO2', &
     & 'MCO3 + R4N1 =  0.75 ALD2 +  0.39 CH2O + MO2 + NO2 +  0.30 R4', &
     & 'MCO3 + MVN2 = CH2O +  0.50 HO2 +  0.50 MCO3 +  0.50 MGLY + M', &
     & 'MAN2 + MCO3 = CH2O + MGLY + MO2 + NO2', &
     & 'INO2 + MCO3 =  0.15 CH2O +  0.85 HNO3 +  0.80 HO2 +  0.10 MA' /

      data (lqkchem(i), i=271,280) / &
     & 'MCO3 + PRN1 = ALD2 + CH2O + MO2 + NO2', &
     & 'MCO3 + R4O2 = ACTA + MEK', &
     & 'ATO2 + MCO3 = ACTA + MEK', &
     & 'KO2 + MCO3 = ACTA + MEK', &
     & 'MCO3 + RIO2 = ACTA + MEK', &
     & 'MCO3 + RIO1 = ACTA + MEK', &
     & 'IAO2 + MCO3 = ACTA + MEK', &
     & 'MCO3 + VRO2 = ACTA + MEK', &
     & 'MCO3 + MRO2 = ACTA + MEK', &
     & 'MCO3 + R4N1 = ACTA + NO2 + RCHO' /

      data (lqkchem(i), i=281,290) / &
     & 'ISN1 + MCO3 = ACTA + NO2 + RCHO', &
     & 'MCO3 + MVN2 = ACTA + NO2 + RCHO', &
     & 'MAN2 + MCO3 = ACTA + NO2 + RCHO', &
     & 'INO2 + MCO3 = ACTA + NO2 + RCHO', &
     & 'MCO3 + PRN1 = ACTA + NO2 + RCHO', &
     & 'B3O2 + MCO3 = ACET + ACTA', &
     & 'ETO2 + MCO3 = ALD2 + HO2 + MO2', &
     & 'ETO2 + MCO3 = ACTA + ALD2', &
     & 'MCO3 + RCO3 = ETO2 + MO2', &
     & 'GCO3 + MCO3 = CH2O + HO2 + MO2' /

      data (lqkchem(i), i=291,300) / &
     & 'MAO3 + MCO3 = CH2O + MCO3 + MO2', &
     & 'N2O5 = 2 HNO3', &
     & 'ClONO2 = HNO3 + HOCl', &
     & 'BrONO2 = HNO3 + HOBr', &
     & 'ClONO2 + HCl = Cl2 + HNO3', &
     & 'HCl + HOCl = Cl2 + H2O', &
     & 'HCl + HOBr = BrCl + H2O', &
     & 'N2O5 = 2 HNO3', &
     & 'ClONO2 = HNO3 + HOCl', &
     & 'BrONO2 = HNO3 + HOBr' /

      data (lqkchem(i), i=301,310) / &
     & 'ClONO2 + HCl = Cl2 + HNO3', &
     & 'HCl + HOCl = Cl2 + H2O', &
     & 'HCl + HOBr = BrCl + H2O', &
     & 'ClONO2 = HNO3 + HOCl', &
     & 'BrONO2 = HNO3 + HOBr', &
     & 'ClONO2 + HCl = Cl2 + HNO3', &
     & 'HCl + HOCl = Cl2 + H2O', &
     & 'BrONO2 + HCl = BrCl + HNO3', &
     & 'HCl + HOBr = BrCl + H2O', &
     & 'ClONO2 = HNO3 + HOCl' /

      data (lqkchem(i), i=311,320) / &
     & 'BrONO2 = HNO3 + HOBr', &
     & 'ClONO2 + HCl = Cl2 + HNO3', &
     & 'HCl + HOCl = Cl2 + H2O', &
     & 'BrONO2 + HCl = BrCl + HNO3', &
     & 'HCl + HOBr = BrCl + H2O', &
     & 'HNO3 = NO2 + OH', &
     & 'NO3 + NO3 = 2 NO2 + O2', &
     & 'HO2 =  0.50 H2O2', &
     & 'NO2 =  0.50 HNO2 +  0.50 HNO3', &
     & 'NO3 = HNO3' /

      data (lqkchem(i), i=321,321) / &
     & 'N2O5 = 2 HNO3' /
!
!.... Photolytic reaction labels
!
      data (lqjchem(i), i=1,10) / &
     & 'O2 + hv = 2 O', &
     & 'O3 + hv = O + O2', &
     & 'O3 + hv = O1D + O2', &
     & 'HO2 + hv = O + OH', &
     & 'H2O + hv = H + OH', &
     & 'NO + hv = N + O', &
     & 'N2O + hv = N2 + O1D', &
     & 'NO2 + hv = NO + O', &
     & 'H2O2 + hv = 2 OH', &
     & 'MP + hv = CH2O + HO2 + OH' /

      data (lqjchem(i), i=11,20) / &
     & 'CH2O + hv = CO + H + HO2', &
     & 'CH2O + hv = CO + H2', &
     & 'HNO3 + hv = NO2 + OH', &
     & 'HNO2 + hv = NO + OH', &
     & 'HNO4 + hv = NO3 + OH', &
     & 'NO3 + hv = NO2 + O3', &
     & 'NO3 + hv = NO + O2', &
     & 'N2O5 + hv = NO2 + NO3', &
     & 'N2O5 + hv = NO + NO3 + O3', &
     & 'HNO4 + hv = HO2 + NO2' /

      data (lqjchem(i), i=21,30) / &
     & 'Cl2 + hv = 2 Cl', &
     & 'OClO + hv = ClO + O', &
     & 'Cl2O2 + hv = 2 Cl + O2', &
     & 'HOCl + hv = Cl + OH', &
     & 'ClONO2 + hv = Cl + NO3', &
     & 'ClONO2 + hv = ClO + NO2', &
     & 'BrCl + hv = Br + Cl', &
     & 'BrO + hv = Br + O', &
     & 'HOBr + hv = Br + OH', &
     & 'BrONO2 + hv = Br + NO3' /

      data (lqjchem(i), i=31,40) / &
     & 'BrONO2 + hv = BrO + NO2', &
     & 'CH3Cl + hv = Cl + MO2', &
     & 'CCl4 + hv = 4 Cl', &
     & 'CH3CCl3 + hv = 3 Cl', &
     & 'CFCl3 + hv = 3 Cl', &
     & 'CF2Cl2 + hv = 2 Cl', &
     & 'CFC113 + hv = 3 Cl', &
     & 'CFC114 + hv = 2 Cl', &
     & 'CFC115 + hv = Cl', &
     & 'HCFC141b + hv = 2 Cl' /

      data (lqjchem(i), i=41,50) / &
     & 'HCFC142b + hv = Cl', &
     & 'CH3Br + hv = Br + MO2', &
     & 'CF3Br + hv = Br', &
     & 'CF2Br2 + hv = 2 Br', &
     & 'H2402 + hv = 2 Br', &
     & 'CF2ClBr + hv = Br + Cl', &
     & 'ALD2 + hv = CO + HO2 + MO2', &
     & 'ALD2 + hv = CH4 + CO', &
     & 'PAN + hv = MCO3 + NO2', &
     & 'RCHO + hv = CO + ETO2 + HO2' /

      data (lqjchem(i), i=51,60) / &
     & 'ACET + hv = MCO3 + MO2', &
     & 'MEK + hv = ETO2 + MCO3', &
     & 'GLYC + hv = CH2O + CO + 2 HO2', &
     & 'GLYX + hv = 2 CO + H2', &
     & 'GLYX + hv = 2 CO + 2 HO2', &
     & 'GLYX + hv = CH2O + CO', &
     & 'MGLY + hv = CO + HO2 + MCO3', &
     & 'MGLY + hv = ALD2 + CO', &
     & 'MVK + hv = CO + PRPE', &
     & 'MVK + hv = CH2O + CO + HO2 + MCO3' /

      data (lqjchem(i), i=61,70) / &
     & 'MVK + hv = MAO3 + MO2', &
     & 'MACR + hv = HO2 + MAO3', &
     & 'MACR + hv =  0.20 CH2O + CO +  1.80 HO2 +  0.20 MCO3 +  0.80', &
     & 'HAC + hv = CH2O + HO2 + MCO3', &
     & 'INPN + hv = HO2 + NO2 + OH + RCHO', &
     & 'PRPN + hv = HO2 + NO2 + OH + RCHO', &
     & 'ETP + hv = ALD2 + HO2 + OH', &
     & 'RA3P + hv = HO2 + OH + RCHO', &
     & 'RB3P + hv = HO2 + OH + RCHO', &
     & 'R4P + hv = HO2 + OH + RCHO' /

      data (lqjchem(i), i=71,80) / &
     & 'PP + hv = HO2 + OH + RCHO', &
     & 'RP + hv = ALD2 + HO2 + OH', &
     & 'GP + hv = CH2O + HO2 + OH', &
     & 'RIP + hv =  0.69 CH2O +  0.86 HO2 +  0.13 IALD +  0.29 MACR ', &
     & 'IAP + hv =  0.67 CO +  0.26 GLYC +  0.19 H2 +  0.36 HAC + HO', &
     & 'ISNP + hv = HO2 + NO2 + OH + RCHO', &
     & 'VRP + hv =  0.28 CH2O +  0.72 GLYC +  0.28 HO2 +  0.72 MCO3 ', &
     & 'MRP + hv =  0.17 CH2O +  0.83 CO +  0.83 HAC + HO2 +  0.17 M', &
     & 'MAOP + hv = HO2 + OH + RCHO', &
     & 'R4N2 + hv =  0.05 A3O2 +  0.32 ACET +  0.32 ALD2 +  0.18 B3O' /

      data (lqjchem(i), i=81,81) / &
     & 'MAP + hv = MO2 + OH' /

!                                  --^--

