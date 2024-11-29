# Load required libraries
library(httr)
library(jsonlite)

# Base URLs
GITHUB_API_BASE <- "https://api.github.com/repos/"
GITHUB_RAW_BASE <- "https://raw.githubusercontent.com/"

# Function to list files/folders in a GitHub repository
list_github_files <- function(repo, path = "") {
    url <- paste0(GITHUB_API_BASE, repo, "/contents/", path)
    response <- GET(url)

    if (response$status_code == 200) {
        fromJSON(content(response, "text", encoding = "UTF-8"))
    } else {
        stop("Failed to fetch repository contents. Check the repository or path.")
    }
}

# Function to download a file from GitHub
download_github_file <- function(repo, file_path, save_path) {
    url <- paste0(GITHUB_RAW_BASE, repo, "/master/", file_path)
    response <- GET(url)

    if (response$status_code == 200) {
        writeBin(content(response, "raw"), save_path)
        message("File downloaded and saved to: ", save_path)
    } else {
        stop("Failed to download the file. Check the file path.")
    }
}

# Ensure the "data" folder exists
ensure_data_folder <- function() {
    if (!dir.exists("data")) {
        dir.create("data")
        message("Created folder: data")
    }
}

# Function to handle user interaction and file download
explore_and_download <- function(repo, base_path) {
    files <- list_github_files(repo, base_path)
    file_names <- files$name
    cat("Files and folders available in the repository:\n")
    print(file_names)

    cat("\nEnter the name of a file or folder to explore/download: ")
    selected_item <- trimws(readline())
    item <- files[files$name == selected_item, ]

    if (nrow(item) == 0) {
        message("Item not found in the repository.")
        return()
    }

    if (item$type == "dir") {
        sub_path <- paste0(base_path, "/", selected_item)
        sub_files <- list_github_files(repo, sub_path)
        sub_file_names <- sub_files$name
        cat("\nExploring folder: ", selected_item, "\n")
        print(sub_file_names)

        cat("\nEnter the name of a file to download: ")
        sub_selected_file <- trimws(readline())
        if (sub_selected_file %in% sub_file_names) {
            ensure_data_folder()
            save_path <- file.path("data", sub_selected_file)
            download_github_file(repo, paste0(sub_path, "/", sub_selected_file), save_path)
        } else {
            message("File not found in the folder.")
        }
    } else if (item$type == "file") {
        ensure_data_folder()
        save_path <- file.path("data", selected_item)
        download_github_file(repo, paste0(base_path, "/", selected_item), save_path)
    }
}

# Main execution
repo <- "caseykneale/ChemometricsData.jl"
base_path <- "data"
explore_and_download(repo, base_path)
