#!/usr/bin/env python3
# Code to generate perturbed initialization files for S2S3
# Requires configuration file: perturbation_config.yaml
# Questions: veronica.i.ruizxomchuk@nasa.gov
# 04/2024

import argparse
from pathlib import Path
from shutil import rmtree #, copy2 
import sys
import re

from datetime import datetime, timedelta
import numpy as np
import xarray as xr

# Functions
##############################################################

def return_season(pdate):
    """
    Return season code based on the given date.
        Input:
            pdate: String (YYYYMMDD) or date object
        Output:
            3-letter code for the season
    """
    if type(pdate) is str:
        try:
            pdate = datetime.strptime(pdate, '%Y%m%d').date()# date object
        except Exception as e:
            logging.error(f"Error reading {pdate}: {e}")
            sys.exit(1)
    seasons = ['DJF', 'MAM', 'JJA', 'SON']
    return seasons[(pdate.month) // 3 % 4]

def get_seps_facs(fname):
    """
    Read Sep and Fac values from the given file.
    Files are usualy given as alpha10_rho_{season}_9ddiffs_V3_{ananame}.txt
    Function is hardcoded for how these files are structured
    """
    logging.info(f"\nGetting Sep, Fac from {fname}")
    try:
        with open(fname, "r") as f:
            lines = f.readlines()
            Sep_match = re.search(r'ddiffs ([\d\s,]+)', lines[0])
            Fac_match = re.search(r'([\de\.\-\s]+)', lines[1])
            if Sep_match and Fac_match:
                Sep = [int(x) for x in Sep_match.group(1).split(',')]
                Fac = [float(x) for x in Fac_match.group(0).split()]
            else:
                raise ValueError("Matching error")
    except FileNotFoundError:
        logging.error(f"File not found: {fname}")
        sys.exit(1)
    except Exception as e:
        logging.error(f"Error reading file {fname}: {e}")
        sys.exit(1)
    return Sep, Fac

def clean_dir(path, **kwargs):
    "Create new dir. Remove previous if exists"
    if path.exists():
        rmtree(path)
    path.mkdir(**kwargs)
    
def clean_file(fpath):
    "Remove file or softlink if exists"
    if fpath.is_file():
        fpath.unlink()

def make_random_date_pairs(pdate, daylim, nummembers, Sep, Fac):
    """ 
    Make random date windows for perturbations:
    Input:
        pdate     : Original date for perturbation. String (YYYYMMDD) or date object
        daylim    : Limit of days back the windows can get
        nummembers: How many pairs of dates to generate
        Sep       : List of numbers of separation dates
        Fac       : List of factors associated with Sep
        allow_rep : True or (False). If allow repetition in the random numbers to extract Sep, Fac
    Output:
        list of tuples: [(odate, odate-Sep, fac), .. ] with size nummembers

    How it works: {nummembers} times
    1. Generates  a random number rn1 between 1 and daylim:
     pd1 = pdate - rn1 days
    2. Generates a random number rn2 between 1 and the number of elements in Sep:
     pd2 = pd1 - Sep[rn2]
    3. If pd2 < pdate - daylim, pd2 = pd1 and pd1 = pdate + Sep[rn2]
    4. Stores a tuple of (pd1, pd2, Fac[rn2])
    
    If nummembers <= len(Sep), rn2 does not allow repetitions
    If nummembers > len(Sep), rn2 can be repeated
    """ 
    logging.info(f'\nGenerating {nummembers} perturbation date pairs (windows) within the past {daylim} days')
    if type(pdate) is str:
        pdate = datetime.strptime(pdate, '%Y%m%d').date() # date object
    bound = pdate - timedelta(days=daylim)
    
    allow_rep = nummembers > len(Sep)
    if allow_rep:
        logging.info(f"Requested members ({nummembers}) is larger than available Separations ({len(Sep)}).\
                         \n Allowing sep repetitions to accomodate.")
        ddelta = np.random.randint(0, daylim,nummembers)
        linenum = np.random.randint(0, len(Sep),nummembers)
    else:
        ddelta = np.random.permutation(daylim)[:nummembers]
        linenum = np.random.permutation(len(Sep))[:nummembers]
    members = []
    logging.info(f"{'Date 1':<10} {'Date 2':<10} {'Sep':>3} {'Frac'}")
    for dd, ln in zip(ddelta, linenum):
        pd1 = pdate - timedelta(days=int(dd+1))
        s, f = Sep[ln], Fac[ln]
        pd2 = pd1 - timedelta(days=int(s))
        if pd2 < bound: # make sure window is within daylim
            pd2 = pd1
            pd1 = pd2 + timedelta(days=s)    
        members.append((pd1.strftime('%Y%m%d'), pd2.strftime('%Y%m%d'), f))
        logging.info(f"{pd1.strftime('%Y%m%d'):<10} {pd2.strftime('%Y%m%d'):<10} {s:>3} {f}")
    return members
    
def perturb_atmo(AtmoDIR, RESDIR, ananame, pdate, Atmfile, daylim=60, members=5, balanced=False):
    """ 
    Generate Atmospheric perturbations
    Input:
        AtmoDIR   : Work directory where perturbations will be saved. Path object.
        RESDIR    : Directory to find restarts.
        ananame   : Experiment name or id.
        pdate     : Original date for perturbations. String (YYYYMMDD) or date object.
        daylim    : Limit of days back the windows can get.
        members   : Number of perturbations to generate.
        Atmfile   : Path to the file containing Sep, Facs for the experiment. String or Path object.
        balanced  : Bool. If False (default), only positive side is computed (see Notes).
    Output:
        Saves perturbation files in AtmoDIR/data_{ananame}/{pdate}/

    Notes: 
     - Discussion on perturbation formulation can be found on 3.1.1 in: 
       Technical Report Series on Global Modeling and Data Assimilation, Volume 53 Randal D. Koster, Editor
       Ensemble Generation Strategies Employed in the GMAO GEOS-S2S Forecast System 
       Siegfried Schubert, Anna Borovikov, Young-Kwon Lim, and Andrea Molod 
       https://gmao.gsfc.nasa.gov/pubs/docs/Schubert1183.pdf.
     - Only on prognostic variables 'Q' (moist) and 'U', 'V' and 'PT' (fvcore)
     - Formulation (positive) is:
        perturbed = file{pdate}, and
        perturbed[variable] = file[variable]{pdate} + frac * (file[variable]{pd1} - file[variable]{pd2})
       Where pd1, pd2 and frac are obtained by make_random_date_pairs function.
     - Frac and Sep are obtained from a file and computed elsewhere.
     - Included a negative portion if balanced is chosen: 
        perturbed[variable] = file[variable]{pdate} - frac * (file[variable]{pd1} - file[variable]{pd2})
    """
    logging.info('\nStarting ATMO section')
    Sep, Fac = get_seps_facs(Atmfile)
    daylim, members = int(daylim), int(members)
    members_list = make_random_date_pairs(pdate, daylim, members, Sep, Fac)
    # NETCDF_DIR = AtmoDIR.joinpath(f'data_{ananame}',f'{pdate}')
    NETCDF_DIR = AtmoDIR.joinpath('output')
    clean_dir(NETCDF_DIR, parents=True)
    # clean_dir(NETCDF_DIR.joinpath(f'ens200'))
    logging.info(f"\nReading Atmo {pdate} restarts")
    restarts = {}
    for name in ['moist', 'fvcore']:
        pfile = RESDIR.joinpath(f'{name}_internal_rst.{pdate}_2100z.nc4')
        # copy2(pfile, NETCDF_DIR.joinpath(f'ens200'))
        logging.info(f"{pfile.name}")
        restarts[name] = xr.open_dataset(pfile)

    logging.info('\nGeneratig perturbations')
    
    for i, nens in enumerate([ f'ens{k+1}' for k in range(members)]):
        PERT_DIR = NETCDF_DIR.joinpath(nens)
        clean_dir(PERT_DIR)
        pd1, pd2, frac = members_list[i]
        logging.info(f"\nWindow {i+1}: Linking {pd1} and {pd2}")
        for name, vnames in zip(['moist', 'fvcore'], [['Q'], ['U', 'V', 'PT']]):
            respd1 = RESDIR.joinpath(f'{name}_internal_rst.{pd1}_2100z.nc4')
            respd2 = RESDIR.joinpath(f'{name}_internal_rst.{pd2}_2100z.nc4')
            logging.info(f"Perturb {name}")
            p0 = restarts[name]
            p1 = xr.open_dataset(respd1)
            p2 = xr.open_dataset(respd2)
            outfile = p0
            if balanced=='True':
                PERT_DIR.joinpath('neg').mkdir()
                outfile_n = p0
            logging.info(f"Getting variables:{vnames}")
            for v in vnames:
                outfile[v] = p0[v] + frac*(p1[v]-p2[v])
                if balanced=='True':
                    outfile_n[v] = p0[v] - frac*(p1[v]-p2[v])
            oname = PERT_DIR.joinpath(f"{name}_internal_rst")
            logging.info(f"Saving {oname}")
            outfile.to_netcdf(oname)
            if balanced=='True':
                oname = PERT_DIR.joinpath('neg',f"{name}_internal_rst")
                logging.info(f"Saving {oname}")
                outfile.to_netcdf(oname)
    logging.info(f"\nFinalized {members} Atmo perturbations for {pdate}\n")


def ScalingFactor(T,lo=-1, up=1):
    """ Calculate scaling factor based on SST with a smoothstep function from 0 to 1 
        between lo and up.
    """
    logging.info('Calculating scaling factor from SST')
    y = (T - lo) / (up - lo)
    sf = y * y * y * (6.0 * y * y - 15.0*y + 10.0)
    sf = sf.where(T >= lo, other= 0)
    sf = sf.where(T <= up, other= 1)
    return sf

def perturb_ocean(OcnDIR, RESDIR, ananame, pdate, Ocnfile, daylim=60, members=10, balanced=False):
    """ 
    Generate Oceanic perturbations
    Input:
        OcnDIR    : Work directory where perturbations will be saved.
        RESDIR    : Directory to find restarts.
        ananame   : Experiment name or id.
        pdate     : Original date for perturbations. String (YYYYMMDD) or date object.
        daylim    : Limit of days back the windows can get.
        members   : Number of perturbations to generate.
        Ocnfile   : Path to the file containing Sep, Facs for the experiment. String or Path object.
        balanced  : Bool. If False (default True), only positive side is computed (see Notes).
    Output:
        Saves perturbation files in OcnDIR/output/'
    Note: 
     - Perturbs all variables in files
     - Formulation is:
       perturbed = file{pdate} + frac * (file{pd1} - file{pd2}) * resfac/2
       Where pd1, pd2 and frac are obtained by make_random_date_pairs function.
       Resfac is a rescaling factor obtained from a sponge-like function (ScalingFactor) aplied over SST to avoid perturbing under ice
    """
    logging.info('\nStarting OCN section')
    RESDIR = RESDIR.joinpath('RESTART')
    Sep, Fac = get_seps_facs(Ocnfile)
    daylim, members = int(daylim), int(members)
    members_list = make_random_date_pairs(pertdate, daylim, members, Sep, Fac)
    # XBVDIR = OcnDIR.joinpath(f'perts_scaleT_{ananame}_V3_{pertdate}_21z')
    XBVDIR = OcnDIR.joinpath('output')
    clean_dir(XBVDIR, parents=True)
    # CDIR = XBVDIR.joinpath('cdata')
    # clean_dir(CDIR)
    PDIR = XBVDIR.joinpath('pdata')
    clean_dir(PDIR)
    if balanced=='True':
        NDIR = XBVDIR.joinpath('ndata')
        clean_dir(NDIR)    
    restarts = {}
    logging.info(f"\nLoading {pertdate} restarts to perturb")
    for ft in ['temp_salt', 'velocity']:
        nc = RESDIR.joinpath(f'{pertdate}_2100z.ocean_{ft}.res.nc')
        if not nc.is_file():
            logging.info(f"REQUIRED {nc} is MISSING")
            sys.exit(1)
        else:
            # copy2(nc, CDIR)
            restarts[ft] = xr.open_dataset(nc)

    resfac = ScalingFactor(restarts['temp_salt']['temp'].isel(zaxis_1=0))

    for i, mtuple in enumerate(members_list):
        logging.info(f"\nMEMBER #{i+1}")
        logging.info(mtuple)
        pd1, pd2, frac = mtuple
        for ft in ['temp_salt', 'velocity']:
            cvar = restarts[ft]
            pvar = xr.open_dataset(RESDIR.joinpath(f'{pd1}_2100z.ocean_{ft}.res.nc'))
            nvar = xr.open_dataset(RESDIR.joinpath(f'{pd2}_2100z.ocean_{ft}.res.nc'))
            if ft == 'temp_salt':
                factor = resfac*frac*.5
            if ft == 'velocity':
                factor = frac*.5  
            #PP
            outfile = cvar + factor*(pvar-nvar)
            oname = PDIR.joinpath(f"{i+1}_ocean_{ft}.res.nc")
            logging.info(f"Saving {oname}")
            outfile.to_netcdf(oname)
            if balanced=='True':
                #NP
                outfile = cvar - factor*(pvar-nvar)*frac*.5
                oname = NDIR.joinpath(f"{i+1}_ocean_{ft}.res.nc")
                logging.info(f"Saving {oname}")
                outfile.to_netcdf(oname)           

def get_logname(ananame, pertdate):
    "Generate name for output log"
    fname = f"{ananame}_{pertdate}.log"
    fout = fname
    while Path(fout).is_file():
        delta = delta+1 if 'delta' in vars() else 1
        fout = fname.split('.')[0]+f'_{delta}.'+fname.split('.')[-1]
    return fout
            
def parserme():
    "Parse input arguments"
    parser = argparse.ArgumentParser(description=("Script to generate perturbations"))
    parser.add_argument('date', metavar="YYYYMMDD", type=int, help=f'Initialization date for perturbations')
    pertdate = str(parser.parse_args().date)
    return pertdate



if __name__ == "__main__":
    startTime = datetime.now()
    import logging
    import yaml
    # LOAD setup file
    ##############################################################    
    config = yaml.load( open('perturbation_config.yaml', "r"), Loader=yaml.BaseLoader)
    ananame = config['paths']['ananame']
    XDIR = config['paths']['XDIR']
    RESDIR = config['paths']['RESDIR']
    atmoconfig = config['limits']['atmo']
    ocnconfig = config['limits']['ocn']
    #dates = config['dates'] # not yet
    # INIT
    ##############################################################
    pertdate = parserme()
    logname = get_logname(ananame, pertdate)
    logging.basicConfig(
        level=logging.INFO,
        format="%(message)s",
        handlers=[
            logging.FileHandler(logname),
            logging.StreamHandler()
        ]
    )
    logging.info("\nStarting perturbation routine for:")
    logging.info(yaml.dump(config['paths']))
    logging.info(yaml.dump(config['limits']))
    season = return_season(pertdate)
    try: 
        avail_ATMdates = config['dates_pool']['ATMpdates'][f'atm{season}']
    except: 
        avail_ATMdates = []
    try: 
        avail_OCNdates = config['dates_pool']['OCNpdates'][f'ocn{season}']
    except: 
        avail_OCNdates = []
    RESDIR = Path(RESDIR)
    # ATMO
    ##############################################################
    if pertdate[4:] in avail_ATMdates or len(avail_ATMdates)==0:
        logging.info(f'{pertdate} in Atmo pool {avail_ATMdates}')
        AtmoDIR = Path(XDIR,'AtmPertsV3')
        Atmfile = AtmoDIR.joinpath(f'alpha10_rho_{season}_9ddiffs_V3_{ananame}.txt')
        perturb_atmo(AtmoDIR, RESDIR, ananame, pertdate, Atmfile, **atmoconfig)
    else:
        logging.info(f'{pertdate} not contaided in Atmo pool {avail_ATMdates}, skipping Atmo perturbations')

    # OCEAN
    ##############################################################
    if pertdate[4:] in avail_OCNdates or len(avail_OCNdates)==0:
        logging.info(f'{pertdate} in Ocn pool {avail_OCNdates}')
        OcnDIR = Path(XDIR,'OcnPertsV3')
        Ocnfile = OcnDIR.joinpath(f'ocn_alpha10_{season}_9ddiffs_5levs_V3_{ananame}.txt')
        perturb_ocean(OcnDIR, RESDIR, ananame, pertdate, Ocnfile, **ocnconfig)
    else:
        logging.info(f'{pertdate} not contaided in Ocn pool {avail_OCNdates}, skipping Ocn perturbations')
    logging.info("\nDONE")
    endTime = datetime.now()
    logging.info(f"Total exec time: {endTime - startTime}")
