
get_author_name <- function()
{
    return(system("git log -1 --pretty=format:'%an'", intern=TRUE))
}

get_author_email <- function()
{
    return(system("git log -1 --pretty=format:'%ae'", intern=TRUE))
}

get_commit_sha1 <- function()
{
    return(system("git rev-parse HEAD", intern=TRUE))
}

get_commit_date <- function(commit_sha)
{
    return(system(paste("git show -s --format=%ci", commit_sha), intern=TRUE))
}

get_remote_url <- function()
{
    return(system("git config --get remote.origin.url", intern=TRUE))
}

get_tag <- function()
{
    return(system("git describe --dirty --tags", intern=TRUE))
}

GitMetadata <- list(
    CommitSHA1 = get_commit_sha1(),
    CommitDate = get_commit_date(get_commit_sha1()),
    AuthorName = get_author_name(),
    AuthorEmail = get_author_email(),
    URL = get_remote_url(),
    TAG = get_tag()
)