# Check, create directory
check_create_dir <- function(dir_path) {
    if(dir.exists(dir_path)) { # if directory DOES exist...
        print("the directory already exists") # ... print this statement
    }
    if(!dir.exists(dir_path)) { # if directory does NOT exist (!)...
        dir.create(dir_path) # ... create the directory and...
        print("the directory has been created") # ...print this statement
    }
}

# subset and copy function
subset.by.time.of.day <- function(from.path, # the directory to extract the images from
                                  to.path, # the new location for the images (it is okay if the folder doesn't exist as the function will create it if needed)
                                  camera.number,  # the camera number will be added to the end of the "from.path" to create the main directory
                                  image.ext, # the file extension possibilities for the images in quotes (e.g. ".JPG" or ".jpg"). Concatenate a list if needed.
                                  start.of.day, # the number of the hour to start daily subset (1 or 2 digits in 24-hour clock: e.g. 4 or 20)
                                  end.of.day) {

# Create list of all files (images, .JPG) for the camera number
image_filelist <- list.files(path = from.path, # get the files from within the "from.path/camera.number" folder
                             recursive = TRUE, # extract all files within subfolders of main path
                             pattern = image.ext, # only look for files ending in the options of `image.ext`
                             full.names = FALSE)

# Create blank list that will hold the subsetted portion of the day: 
filelist_daily_subset <- as.list(c()) 

# For loop to create list of files taken between 9 am and 1 pm:
for(i in image_filelist) { # for all files in wsn file list
    QAcheck <- grepl("L__", i) # check that the names have been changed to include a date
    if(QAcheck == TRUE){ # if the name includes "L__", then ignore and continue to the next object i)
        next
    }
    QAcheck2 <- grepl("1).JPG", i) # check that there are not duplicates
    if(QAcheck2 == TRUE){ # if the name includes "1).JPG", then ignore and continue to the next object i)
        next
    }
    hour <- hour(extractDateFilename(i,
                                     date.code = "yyyy_mm_dd_HHMM")) # extract the hour from the file name
    if(hour >= start.of.day & hour < end.of.day) { # if the hour is >= 9 or < 13, then... 
        filelist_daily_subset <- c(filelist_daily_subset, i) # add (concatenate) the object into  wsn09_filelist_hour9to13
    }
}

check_create_dir(dir_path = paste0(to.path, "/IMG_full_season/"))

# Copy files from `filelist_daily_subset` list to new location:

for(i in filelist_daily_subset) {
    temp.to <- paste0(to.path, "/IMG_full_season")
    temp.from <- paste(from.path, "/", i, sep="")
    file.copy(temp.from, temp.to)    
    }
}

#### Subset by DATE
subset.by.date <- function(from.path, # the directory to extract the images from
                           to.path, # the new location for the images (it is okay if the folder doesn't exist as the function will create it if needed)
                           camera.number, # the camera number will be added to the end of the "from.path"
                           fov.number, # the number of the frame of views
                           image.ext, # the file extension possibilities for the images in quotes (e.g. ".JPG" or ".jpg"). Concatenate a list if needed.
                           start.date, # the number of the hour to start daily subset (1 or 2 digits in 24-hour clock: e.g. 4 or 20)
                           end.date) {
    
    # Create list of all files (images, .JPG) in the RawFieldData folder
    image_filelist <- list.files(path = paste0(from.path, camera.number), # get the files from within the "from.path/camera.number" folder
                                 recursive = TRUE, # extract 
                                 pattern= image.ext, # only look for files ending in the options of `image.ext`
                                 full.names = FALSE)
    
    # Create blank list that will hold the subsetted portion of the day: 
    filelist_subset <- as.list(c()) 
    
    # For loop to create list of files taken between dates of interest:
    for(i in image_filelist) { # for all files in wsn file list
        QAcheck <- grepl("/L__", i) # check that the names have been changed to include a date
        if(QAcheck == TRUE){ # if the name includes "L__", then ignore and break loop (i.e. continue to the next object i)
            break
        }
        QAcheck2 <- grepl("(1)", i) # check that there are not duplicates (indicated by a "(1)" after the name)
        if(QAcheck2 == TRUE){ # if the name includes "(1)", then ignore and break loop (i.e. continue to the next object i)
            break
        }
        date <- date(extractDateFilename(i,
                                         date.code = "yyyy_mm_dd_HHMM")) # extract the date from the file name
        if(date >= start.date & date < end.date) { # if the date is >= start.date or < end.date, then... 
            filelist_subset <- c(filelist_subset, i) # add (concatenate) the object into subset_filelist
        }
    }

    
    # Copy files from `filelist_subset` list to new location:
    
    for(i in filelist_subset) {
        # temp.to <- paste0(to.path, camera.number, "/FOV", fov.number, "/IMG/")
        # temp.from <- paste(from.path, camera.number, "/", i, sep="")
        file.copy(from.path, to.path)
    }
}