function statmak (args)
field  = subwrd  (args,1)
tag    = subwrd  (args,2)
ctl    = subwrd  (args,3)

'numargs  'args
 numargs = result

say ' '
say 'Inside STATMAK, field: 'field
say '                  tag: 'tag
say '                  ctl: 'ctl
say ' '

'run getenv "GEOSUTIL"'
             geosutil = result

if( field = q )
    scale = 1000
else
    scale = 1
endif

* Get Total Number of Files for Subset Experiments
* ------------------------------------------------------------------------
'getinfo numfiles'
         numfiles = result

* Query Level Environment
* -----------------------
'getinfo zfreq'
         zfreq = result

if( zfreq = 'fixed' )
   'getinfo level'
            levmin = result
            levmax = result

   'open 'geosutil'/plots/grads_util/lwmask1440721.tabl'
   'getinfo numfiles'
            newfile = result

   'set dfile 'newfile
   'set t 1'
   'set z 1'
   'define mask = lwmask'
   'close 'newfile
   'set dfile 1'
endif

* Set Proper DFILE for Varying Levels
* -----------------------------------
if( zfreq = 'varying' )
  'getinfo zmin'
           zmin = result
  'getinfo zmax'
           zmax = result
  'set z ' zmin
  'getinfo level'
           levmin = result
  'set z ' zmax
  'getinfo level'
           levmax = result
endif

* Get Proper File for TINC or ZMIN
* --------------------------------
'run getenv "TINCFILE" '
             tincfile = result
'open '      tincfile
'getinfo     numfiles'
             newfile = result
'set dfile ' newfile
'set t 1'
'setlons'
'sety'
'set lev 'levmin' 'levmax

if( zfreq = 'varying' )
   'close 'newfile
   'run getenv "ZMINFILE" '
                zminfile = result
   'open '      zminfile
   'getinfo     numfiles'
                newfile = result
   'set dfile ' newfile
   'set lev 1000 100'
endif


* Define Number of Forecast Days and Time Interval (hrs)
* ------------------------------------------------------

'run getenv "SYSCMP_TINC"'
                    tinc  = result
'run getenv "SYSCMP_TDIM"'
                    tdim  = result
                    tdum  = tdim - 1
                    ndaymax = tdum * tinc / 24
                       nmax = 1 + ndaymax*(24/tinc)

'run getenv "NDAY"'
             nday = result
         if( nday = "NULL" ) ; nday = ndaymax ; endif
         if( nday > ndaymax) ; nday = ndaymax ; endif

         tbeg = 1-(nmax-tdim)
'set t ' tbeg ' 'tdim

'q file'
say 'Default File: 'result
'q dims'
say 'Default File DIMS: 'result


********************************************************************************
****                                                                        ****
**** Note:  Forecast           =>  F                                        ****
****        Analysis           =>  A                                        ****
****                                                                        ****
****        Mean Square Error  =>  MSE  =   1/N * SUM[ (F-A)**2 ]           ****
****        Mean Error         =>  BIAS =   1/N * SUM[ (F-A) ]              ****
****        Mean Error Squared =>  MES  =   BIAS**2                         ****
****        Root Mean  Square  =>  RMS  = SQRT[ MSE ]                       ****
****        Variance           =>  VAR  =   1/N * SUM[ (F-A-BIAS)**2 ]      ****
****        Standard Deviation =>  STD  = SQRT[ VAR ]                       ****
****                                                                        ****
****        Mean Square Error  =   Variance + BIAS**2                       ****
****                      MSE  =   VAR      + MES                           ****
****                                                                        ****
********************************************************************************

* Compute forecast statistics
* ---------------------------
*  fma: forecast minus analysis
*  fmc: forecast minus climatology
*  mes: mean error squared
*  mse: mean square error
*  rms: root mean square error
*  std: standard deviation
* --------------------------------------------------

* Define FMA variables
* --------------------
say 'Defining FMA variables for Field: 'field' and tag: 'tag
say '--------------------------------- '
 
        n  = 1
while ( n <= numfiles )

* Note: Add and Subtract uf & vf to force similar UNDEF locations
* ---------------------------------------------------------------
   if( field = chi )
       'define  uaa'n' = ua.'n'+uf.'n'-uf.'n
       'define  vaa'n' = va.'n'+vf.'n'-vf.'n
       'define chif'n' = fish_chi(uf.'n',vf.'n')'
       'define chia'n' = fish_chi(uaa'n',vaa'n')'
       'define chif'n' = chif'n'-aave(chif'n',g)'
       'define chia'n' = chia'n'-aave(chia'n',g)'
   endif
   if( field = psi )
       'define  uaa'n' = ua.'n'+uf.'n'-uf.'n
       'define  vaa'n' = va.'n'+vf.'n'-vf.'n
       'define psif'n' = fish_psi(uf.'n',vf.'n')'
       'define psia'n' = fish_psi(uaa'n',vaa'n')'
       'define psif'n' = psif'n'-aave(psif'n',g)'
       'define psia'n' = psia'n'-aave(psia'n',g)'
   endif
   if( field  = chi | field  = psi )
       'define f = 'field'f'n
       'define a = 'field'a'n
   else
       'define f = 'field'f.'n
       'define a = 'field'a.'n
   endif

  'define 'field'fma'tag''n'  = (f-a)*'scale

  'define 'field'Xmse'tag''n' = pow( 'field'fma'tag''n',2 )'

   n = n + 1
endwhile


* Compute MeanErrorSquared MES and MeanSquareError MSE for fma
* ------------------------------------------------------------
       'define 'field'fma1'tag' = lat-lat+lon-lon'
       'define 'field'fma2'tag' = lat-lat+lon-lon'
        n  = 1
while ( n <= numfiles )
       'define 'field'fma1'tag' = 'field'fma1'tag' +      'field'fma'tag''n
       'define 'field'fma2'tag' = 'field'fma2'tag' + pow( 'field'fma'tag''n',2 )'
        n = n + 1
endwhile
       'define 'field'fma1'tag' = 'field'fma1'tag' / 'numfiles
       'define 'field'fma2'tag' = 'field'fma2'tag' / 'numfiles

       'define 'field'Xmes'tag' = pow( 'field'fma1'tag',2 )'
       'define 'field'Xmse'tag' =      'field'fma2'tag


* Compute Variance VAR for fma
* ----------------------------
        n  = 1
while ( n <= numfiles )
       'define 'field'Xvar'tag''n' = pow( 'field'fma'tag''n'-'field'fma1'tag',2 )'
        n = n + 1
endwhile

       'define 'field'Xvar'tag' = lat-lat+lon-lon'
        n  = 1
while ( n <= numfiles )
       'define 'field'Xvar'tag' = 'field'Xvar'tag' + 'field'Xvar'tag''n
        n = n + 1
endwhile
       'define 'field'Xvar'tag' = 'field'Xvar'tag' / 'numfiles

* --------------------------------------------------------------
* --------------------------------------------------------------

* Define Zonal-Mean FMA variables
* -------------------------------
if( zfreq = 'varying' )
    say 'Defining Zonal-Mean FMA variables for Field: 'field' and tag: 'tag
    say '-------------------------------------------- '

   'makez 'field'Xmes'tag' z'
   'makez 'field'Xmse'tag' z'
   'makez 'field'Xvar'tag' z'

else
    say 'Skipping: Defining Zonal-Mean FMA variables for Field: 'field' and tag: 'tag
    say '------------------------------------------------------ '
endif

* --------------------------------------------------------------
* --------------------------------------------------------------

* Compute TAG & CTL Variables: X & Y, and Diff Variable: Z = X-Y
* For: mean-error-squared(mes), mean-square-error(mse), variance(var)
* -------------------------------------------------------------------
if( tag != ctl )
    say 'Computing Difference Variables for Field: 'field' and TAGs: 'tag' and 'ctl
    say '----------------------------------------- '

* Define Difference-Variables
* ---------------------------
           n  = 1
   while ( n <= numfiles )
          'define 'field'Dmse'tag''n' = 'field'Xmse'tag''n' - 'field'Xmse'ctl''n
          'define 'field'Dvar'tag''n' = 'field'Xvar'tag''n' - 'field'Xvar'ctl''n
          'define 'field'Dmes'tag''n' = 'field'Xmes'tag'    - 'field'Xmes'ctl
           n = n + 1
   endwhile

* Define D-Variable Means
* ----------------------
          'define 'field'Dmse'tag' = lat-lat+lon-lon'
          'define 'field'Dvar'tag' = lat-lat+lon-lon'
          'define 'field'Dmes'tag' = lat-lat+lon-lon'
           n  = 1
   while ( n <= numfiles )
          'define 'field'Dmse'tag' = 'field'Dmse'tag' + 'field'Dmse'tag''n
          'define 'field'Dvar'tag' = 'field'Dvar'tag' + 'field'Dvar'tag''n
          'define 'field'Dmes'tag' = 'field'Dmes'tag' + 'field'Dmes'tag''n
           n = n + 1
   endwhile
          'define 'field'Dmse'tag' = 'field'Dmse'tag' / 'numfiles
          'define 'field'Dvar'tag' = 'field'Dvar'tag' / 'numfiles
          'define 'field'Dmes'tag' = 'field'Dmes'tag' / 'numfiles

* Define Variances of MSE Differences
* -----------------------------------
          'define 'field'DDmse'tag' = lat-lat+lon-lon'
           n  = 1
   while ( n <= numfiles )
          'define 'field'DDmse'tag' = 'field'DDmse'tag' + pow( 'field'Dmse'tag''n'-'field'Dmse'tag',2 )'
           n = n + 1
   endwhile
          'define 'field'DDmse'tag' = 'field'DDmse'tag' / 'numfiles

* --------------------------------------------------------------
* --------------------------------------------------------------

if( zfreq = 'varying' )
   say 'Computing Zonal Mean of Difference Variables for Field: 'field' and TAGs: 'tag' and 'ctl
   say '------------------------------------------------------ '
   'makez 'field'Dmes'tag'  z'
   'makez 'field'Dmse'tag'  z'
   'makez 'field'Dvar'tag'  z'

           n  = 1
   while ( n <= numfiles )
          'makez 'field'Dmse'tag''n' z'
           n = n + 1
   endwhile

          'set lon 0'
          'define 'field'DDmse'tag'z = lat-lat'
           n  = 1
   while ( n <= numfiles )
          'define 'field'DDmse'tag'z = 'field'DDmse'tag'z + pow( 'field'Dmse'tag''n'z-'field'Dmse'tag'z,2 )'
           n = n + 1
   endwhile
          'define 'field'DDmse'tag'z = 'field'DDmse'tag'z / 'numfiles
endif

* --------------------------------------------------------------
* --------------------------------------------------------------

* End CTL Test
* ------------
endif
'close 'newfile


'set dfile 1'
'set t 'tbeg' 'tdim
'setlons'
'sety'
'set lev 'levmin' 'levmax

return
