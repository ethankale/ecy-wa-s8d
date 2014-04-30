ecy-wa-s8d
==========

Compilation and analysis of data collected under the WA municipal stormwater permit, 2007-2013.

The single .r file in the main directory (Plot_byParam_ver9_[date].r) is the primary script.  It processes the main data set, which is also in the main directory (FinalMasterFile_[date].csv).  Each subsequent r script in the "scripts" directory is either called by the main script, or depends upon it (must be executed within the same work session).

The "output" directory contains all PDFs and CSV files created by the scripts in this project.