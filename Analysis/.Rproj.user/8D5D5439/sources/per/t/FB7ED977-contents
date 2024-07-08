library(jsonlite)  # Make sure to load the jsonlite library for JSON parsing

# Function to find Dropbox path on Windows
find_dropbox_path <- function() {
    # Define the path to the Dropbox info.json file
    info_json_path <- file.path(Sys.getenv("LOCALAPPDATA"), "Dropbox", "info.json")
    
    # Check if the file exists
    if (file.exists(info_json_path)) {
        # Read the JSON file
        info <- jsonlite::fromJSON(info_json_path)
        # Check for the 'personal' and 'path' fields in the JSON
        if ("personal" %in% names(info) && "path" %in% names(info$personal)) {
            return(info$personal$path)
        } else {
            stop("Dropbox personal path not found in info.json")
        }
    } else {
        stop("Dropbox info.json does not exist at the expected location.")
    }
}

# # Try to find the Dropbox path
dropbox_path <- find_dropbox_path()
# print(dropbox_path)  # To check the path

library(servr)

# Append the specific path within Dropbox
dir_to_serve <- file.path(dropbox_path, "CSVS", "OB Service", "Analysis")

# Start an HTTP server on a specified port, such as 3333, without opening a browser
servr::httd(dir = dir_to_serve, port = 3333, browser = FALSE)

#To stop the server, run servr::daemon_stop(1) or restart your R session