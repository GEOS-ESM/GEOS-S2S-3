% Read Argo Grey List text file
% Write wmo, date, var in a text file for fortran
% ftp://ftp.ifremer.fr/ifremer/argo/ar_greylist.txt

clear all

% WMO, VAR, DATE,empty,QC,comment, dac
fname = 'argo_greylist.txt';
fid   = fopen(fname,'r');

tm = 1082;
wmo   = zeros(tm,1);
ivar  = zeros(tm,1);
idate1 = zeros(tm,1);
idate2 = zeros(tm,1);

  tmp = 'a';
  i = 1;
  while i>0,
    line = fgets(fid);
    str  = find(line==',');
    if isempty(str)==1, break; end 
    wmo(i) = str2num(strtrim(line(1:str(1)-1)));  
    tmp = line(str(1)+1:str(2)-1);ivar(i) = 1;   
      if strcmp(tmp,'TEMP')==1, 
        ivar(i) = 1;
      elseif strcmp(tmp,'PSAL')==1, 
        ivar(i) = 2;
      elseif strcmp(tmp,'PRES')==1, 
        ivar(i) = 3;
      end
    idate1(i) = str2num(line(str(2)+1:str(3)-1));    
    tmp = str2num(line(str(3)+1:str(4)-1)); 
    if isempty(tmp)==0,
      idate2(i) = tmp;    
    else
      idate2(i) = 20201231;  
    end
    i=i+1;  
  end
  nrows = i-1;
status = fclose(fid);

% Sort by date
  sv = sortrows([wmo ivar idate1 idate2],1);

% Write new file
  fid = fopen('ARGO_GREYLIST.txt','w');
  for i=1:nrows,
     fprintf(fid,'%-7d%-1d%-8d%-8d\n',[sv(i,1), sv(i,2), sv(i,3), sv(i,4)]);
  end

uwmo = unique(wmo);
save ARGO_GREYLIST.mat uwmo
