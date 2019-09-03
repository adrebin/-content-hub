# Deploy site to either develop or production
deploy_site <- function() {
  version_menu <- menu(c("Develop", "Production", "Abort"),
    title = "Select a version to deploy to:"
  )
  version <- switch(version_menu, "develop", "production", "abort")

  if(version == "abort"){
    return("Aborting deployment")
  }
  else if (version == "production") {
    select <- menu(c("Yes", "No"),
      title = "Are you sure you want to deploy to production?"
    )
    if (select == 2) {
      return("Aborting deployment")
    }
  }

  deploy_data <- config::get(file = "deploy_config.yml", config = version)
  rsconnect::deployApp(
    appDir = "_site", # the directory containing the content
    appPrimaryDoc = deploy_data$appPrimaryDoc, # the primary file
    appName = deploy_data$appName, # name of the endpoint (unique to your account on Connect)
    appTitle = deploy_data$appTitle, # display name for the content
    appId = deploy_data$appId,
    account = deploy_data$account,
    contentCategory = "site",
    recordDir = ".",
    server = deploy_data$server # the Connect server, see rsconnect::accounts()
  )
  return(paste("Successfully deployed to", version))
}

deploy_site()
