% Read Argo ListOfFloatsToBeChecked Lists
% Write wmo, date, var in a text file for fortran
% ftp://ftp.ifremer.fr/ifremer/argo/etc/argo-ast9-item13-AltimeterComparison
% 116+124+111+111+113=575
%clear all

% DAC, WMO, INST, ANOMALIES, RT/DT, CYCLES
  tm = 575;
  wmo   = zeros(tm,1);
  inst  = zeros(tm,1);
  i = 1;

fnames = ['Floats_012009.txt';'Floats_042009.txt';'Floats_062009.txt';'Floats_082009.txt';'Floats_092009.txt'];
for j=1:size(fnames,1),
  fname = fnames(j,:);
  fid   = fopen(fname,'r');
  while i>0,
    line = fgets(fid);
    if isempty(line(1))==1 | line(1)==-1, break; end
    ichar  = isletter(line);
    line2  = line(find(ichar==0));
    tmp2   = isstrprop(line2,'wspace');
    tmp3   = find(tmp2==0);
    s1 = tmp3(1);
    for k=2:length(tmp3),
      if tmp3(k)==tmp3(k-1)+1,
        e1 = tmp3(k);
      else
         break
      end
    end
    s2 = tmp3(k);
    kk = k+1;
    for k=kk:length(tmp3),
      if tmp3(k)==tmp3(k-1)+1,
        e2 = tmp3(k);
      else
        break
      end
    end
    wmo(i)  = str2num(line2(s1:e1));
    inst(i) = str2num(line2(s2:e2));
    if inst(i) < 100, error('inst num');end
    i=i+1;  
  end
  status = fclose(fid);
end
nrows = i-1;
guwmo = unique(wmo);
nrows = length(guwmo);

% Write new file
  fid = fopen('ARGO_CHECKLIST.txt','w');
  for i=1:nrows,
    fprintf(fid,'%-7d\n',[guwmo(i)]);
  end

% Lets get the set of WMO's that are in the CheckList but NOT in the GreyList
  load ARGO_GREYLIST.mat
  wmodiff = setdiff(guwmo,uwmo);
  nrows = length(wmodiff);
% Write new file
  fid = fopen('ARGO_CHECKLIST_DIFF.txt','w');
  for i=1:nrows,
    fprintf(fid,'%-7d\n',[wmodiff(i)]);
  end


