%clear all
format compact
warning off
addpath /gpfsm/dnb42/projects/p16/ssd/ocean/kovach/codes/general/

% matlab_r2011b
  addpath /gpfsm/dgen/mathworks/matlab_r2009a/toolbox/matlab/netcdf_toolbox/netcdf

%pathfinal    = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/assim/PIRATA/V3/FINAL/';
%pathfinal    = '/gpfsm/dnb78s2/projects/p26/ehackert/TAO_PIRATA_RAMA_processing/MOOR/PIRATA/V3/FINAL/';

pathfinal    = '/discover/nobackup/lren1/pre_proc/NRT/MOOR/PIRATA/V3/FINAL/';

syear
eyear

for iyear=syear:eyear,
  miss = 999999;
  newmiss = 9.99e+11;
  syear = num2str(iyear);
  iyear;

  fnames=[pathfinal,'SYN_PIR_',syear,'.nc'];
%    s_cdf = netcdf(fnames,'read');

    ss = ncread(fnames, 'SALT'); ss=ss';
    ys = ncread(fnames, 'LAT');
    xs = ncread(fnames, 'LON');
    zs = ncread(fnames, 'DEPTH');zs=zs';
    qs = ncread(fnames, 'QC_LEV');qs=qs';
    stime = ncread(fnames, 'DATE_TIME');

  fnamet=[pathfinal,'T_PIR_',syear,'.nc'];
%    t_cdf = netcdf(fnamet,'write');

    tt = ncread(fnamet,'TEMP');tt=tt';
    yt = ncread(fnamet,'LAT');
    xt = ncread(fnamet,'LON');
    zt = ncread(fnamet,'DEPTH');zt=zt';
    qt = ncread(fnamet,'QC_LEV');qt=qt';
    qp = ncread(fnamet,'QC_PRF');
    ttime = ncread(fnamet,'DATE_TIME');
    N  = ncread(fnamet,'NPTS');

    if (isequal(size(tt),size(ss)))==1,
      else error('files are different');
    end
    [nobs,npts] = size(tt);
    pt = zeros(nobs,npts);   pt(:,:) = miss;
 
 cntmiss = 0;
    for i=1:nobs,
        for j=1:N(i),
          if tt(i,j) < miss,
            if ss(i,j) < miss,
              pres   = sw_pres(zt(i,j),yt(i));
              pt(i,j) = sw_ptmp(ss(i,j),tt(i,j),pres,0);
            else
              pt(i,j) = newmiss;
              qt(i,j) = 9;
              %disp(['SMISS: ',num2str(ttime(i)),' ',num2str(xt(i)),' ',num2str(yt(i))])
            end % sgood
            if abs(tt(i,j)-pt(i,j))>= 0.1 & qt(i,j)==1,
              error(['ERROR: ',num2str(ttime(i)),' ',num2str(tt(i,j)),' ',num2str(pt(i,j))])
            end
          end % t good
        end % npts
        if min(qt(i,:))==9,
          qp(i) = 9;
          cntmiss = cntmiss+1;
          disp(['PRF MISS: ',num2str(ttime(i)),' ',num2str(xt(i)),' ',num2str(yt(i))])
        end
     end % nobs
%[nobs cntmiss]
%[unique(qt)]
%[unique(qs)]
%[unique(qp)]

ncwrite(fnamet,'TEMP',pt');
ncwrite(fnamet,'QC_LEV',qt');
ncwrite(fnamet,'QC_PRF',qp');

clear s* t* y* z* N nobs npts pt* pr* q*

end

