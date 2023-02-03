#Test and tutorial for review package.

#Welcome to the 'review' package!
#Try this on a 'Nix system that has svn installed.
#First, we navigate to root, blow away "drug" dir if exists,
# and then recreate it.
setwd("/")
system("rm -rf /drug")
system("svnadmin create drug")

#Now we go to your project directory, and check out 'drug'.
setwd("~/project")
system("rm -rf drug")
system("svn co file:///drug")
#Checked out revision 0.
setwd("drug")

#Next we populate the directory with experimental files, etc.
dir.create("data")
dir.create("script")
writeLines("0","data/drug.csv")
writeLines("#master","script/master.R")
writeLines("#assemble","script/assemble.R")
system("svn add *")
#A         data
#A         data/drug.csv
#A         script
#A         script/assemble.R
#A         script/master.R

system("svn ci -m 'draft project'")
#Adding         data
#Adding         data/drug.csv
#Adding         script
#Adding         script/assemble.R
#Adding         script/master.R
#Transmitting file data ...
#Committed revision 1.

#Then we'll load the review package.
library(review)

#Let's create a review log.  This must happen first, and 
# if you're not in the top level directory, you should pass
# its path as an argument.
logCreate()

#Where is this log rooted, and what is its name?
logRoot()#"/Users/timb/project/drug"
logName()#"/Users/timb/project/drug/QClog.csv"

#We check in the log....
system("svn add QClog.csv")
#A         QClog.csv

system("svn ci -m 'new' QClog.csv")
#Adding         QClog.csv
#Transmitting file data .
#Committed revision 2.

#from the perspective of the root directory, we can see 
# all the files that could be reviewed.
dir(recursive=TRUE)
#[1] "QClog.csv"         "data/drug.csv"     "script/assemble.R"
#[4] "script/master.R" 

#Navigate to the script directory.  The assign() function will find
# just those files, by default.
setwd("script")
dir()
#Now we assign a file; essentially we are claiming that the reviewer (default:anyone)
# accepts revision zero.
logAssign()
logAssignments()#character(0)
logAssignments(reviewer=NULL)
#[1] "script/assemble.R" "script/master.R"  

#Every file has an "origin": itself by default.  But since
# our data file was created by our assembly script, that is its origin.
setwd("..")
logAssign("data/drug.csv",origin="script/assemble.R")
logAccept("script/master.R")
logRead()
#               file            origin revf revo reviewer                    time
#1 script/assemble.R script/assemble.R    0    0   anyone 2010-01-16 03:45:18 GMT
#2   script/master.R   script/master.R    0    0   anyone 2010-01-16 03:45:18 GMT
#3     data/drug.csv script/assemble.R    0    0   anyone 2010-01-16 03:45:18 GMT
#4   script/master.R   script/master.R    1    1     timb 2010-01-16 03:45:25 GMT

#Any version of a file can be accepted an arbitrary number of times.
#You can even change the origin using arguments to logAccept() (the most recent wins).
#We can summarize the log, to see just the latest record per file.
logSummary()
#               file            origin revf headf revo heado reviewer                    time
#3     data/drug.csv script/assemble.R    0     1    0     1   anyone 2010-01-16 03:45:18 GMT
#1 script/assemble.R                      0     1              anyone 2010-01-16 03:45:18 GMT
#4   script/master.R                      1     1                timb 2010-01-16 03:45:25 GMT

#Non-informative columns are not displayed.
logSummary()[-1,]
#               file revf headf reviewer                    time
#1 script/assemble.R    0     1   anyone 2010-01-16 03:45:18 GMT
#4   script/master.R    1     1     timb 2010-01-16 03:45:25 GMT

#Pending files have been assigned, but not accepted.  Or if accepted, they or their origins have 
#been revised.
logPending()
#               file            origin revf headf revo heado reviewer                    time
#3     data/drug.csv script/assemble.R    0     1    0     1   anyone 2010-01-16 03:45:18 GMT
#1 script/assemble.R                      0     1              anyone 2010-01-16 03:45:18 GMT

#If we accept these files, they are no longer pending.
logAccept("script/assemble.R")
logAccept("data/drug.csv")
logPending()
#[1] file     revf     headf    reviewer time    
#<0 rows> (or 0-length row.names)

#Check them in.
system("svn ci -m 'qc complete'")
#Sending        QClog.csv
#Transmitting file data .
#Committed revision 3.

#Oh, my.  There's an error in the assembly script.  Let's fix it, and check it in.
writeLines("#reassemble","script/assemble.R")
system("svn ci -m 'change assembly'")
#Sending        script/assemble.R
#Transmitting file data .
#Committed revision 4.

system("svn up")
#At revision 4.

#Not only is assemble.R now pending, but so is the file it creates (by virtue of its origin).
logPending()
#               file            origin revf headf revo heado reviewer                    time
#6     data/drug.csv script/assemble.R    1     1    1     4     timb 2010-01-16 03:48:09 GMT
#5 script/assemble.R                      1     4                timb 2010-01-16 03:48:01 GMT

#Even if we accept the new version of assemble.R, the dataset is still pending, since it was last 
# accepted before assemble.R changed (even though the change did not affect it).
logAccept("script/assemble.R")
logPending()
#           file            origin revf headf revo heado reviewer                    time
#6 data/drug.csv script/assemble.R    1     1    1     4     timb 2010-01-16 03:48:09 GMT

#Finally, we accept the new version of drug.csv.
logAccept("data/drug.csv")
logPending()
#[1] file     revf     headf    reviewer time    
#<0 rows> (or 0-length row.names)

system("svn ci -m 'Done.'")
#Sending        QClog.csv
#Transmitting file data .
#Committed revision 5.

#Some tools exist to let you create logs manually.
gmt() #[1] "2010-01-16 03:51:47 GMT"
logQueue(dir("script"),directory="script")
#               file            origin revf revo reviewer                    time
#1 script/assemble.R script/assemble.R    0    0   anyone 2010-01-16 03:52:47 GMT
#2   script/master.R   script/master.R    0    0   anyone 2010-01-16 03:52:47 GMT

#Create a table suitable for an appendix.
logAppendix()

#see also
?review