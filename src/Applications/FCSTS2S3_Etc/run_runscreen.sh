#!/usr/bin/env bash


screen -dmS rungcmsetup     bash -c './run_rungcmsetup.sh   >> message/stderr_rungcmsetup    2>&1' 
screen -dmS runpstq2q       bash -c './run_runpstq2q.sh     >> message/stderr_runpstq2q      2>&1' 
screen -dmS runsubmit       bash -c './run_runsubmit.sh     >> message/stderr_runsubmit      2>&1' 
screen -dmS runarchstatus   bash -c './run_runarchstatus.sh >> message/stderr_runarchstatus 2>&1' 
screen -dmS rungetstatus    bash -c './run_rungetstatus.sh  >> message/stderr_rungetstatus   2>&1' 
screen -dmS runmkdata       bash -c './run_runmkdata.sh     >> message/stderr_runmkdata      2>&1' 
screen -dmS runmonitor      bash -c './run_runmonitor.sh    >> message/stderr_runmonitor     2>&1' 
screen -dmS runpckwinners   bash -c './run_runpckwinners.sh >> message/stderr_runpckwinners  2>&1' 
screen -dmS runutility      bash -c './run_runutility.sh    >> message/stderr_runutility     2>&1' 

#screen -dmS runrstlfe2pfe   bash -c './run_runrstlfe2pfe.sh >> message/stderr_runrstlfe2pfe  2>&1' 
