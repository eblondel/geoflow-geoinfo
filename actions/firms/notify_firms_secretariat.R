notify_firms_secretariat <- function(action, entity, config){
	opts <- action$options
	
	if(!requireNamespace("blastula", quietly = TRUE)){
	  stop("The 'notify_firms_secretariat' action requires the 'blastula' package")
	}
	#shortcut for SMTP config
	SMTP <- config$software$output$smtp
	
	if(is.null(SMTP)){
	  errMsg <- "This action requires a Geonetwork software to be declared in the configuration"
	  config$logger.error(errMsg)
	  stop(errMsg)
	}
	
	email <- blastula::compose_email(
        header = md("<a href=\"http://firms.fao.org\"><img src=\"http://firms.fao.org/figis/website/assets/images/templates/firms/firms_banner.gif\" height=54 width=800/></a>"), 
        body = md(glue::glue(
            "
Dear FIRMS team member,

The FIRMS stocks & fisheries map viewer **{ifelse(entity$resources$is_review, '(IN REVIEW)', '')}** has been updated with a new **{entity$resources$firmsDomainName}** dataset.

The new dataset (as of {Sys.Date()}) can be browsed in the [FIRMS stocks & fisheries map viewer]({entity$resources$host}/fishery/geoserver/factsheets/firms.html?layer={entity$resources$firmsDomain}). 

{ifelse(!entity$resources$is_review, paste0('The public dataset is accessible for download in the [FAO Fisheries and Aquaculture Division Geonetwork](https://www.fao.org/fishery/geonetwork?uuid=',entity$identifiers$id,')'),'')}


## Summary

* Number of FIRMS {entity$resources$firmsDomainName} placemarks: {nrow(entity$resources$result)} (previously: {nrow(entity$resources$result_old)}) 
* Number of FIRMS {entity$resources$firmsDomainName} placemarks by **type**:

{entity$resources$result_by_type_table}

* Number of FIRMS {entity$resources$firmsDomainName} placemarks by **agency**:

{entity$resources$result_by_agency_table}

## Details

### New records

{ifelse(nrow(entity$resources$new_records)>0, paste0('For download see attached file: ', entity$resources$new_records_filename), '')}

{ifelse(nrow(entity$resources$new_records)>0, entity$resources$new_records_table, 'No new records')}

<br>

### Deleted/Unpublished records

{ifelse(nrow(entity$resources$deleted_records)>0, paste0('For download see attached file: ', entity$resources$deleted_records_filename), '')}

{ifelse(nrow(entity$resources$deleted_records)>0, entity$resources$deleted_records_table, 'No deleted/unpublished records')}

<br>

### Updated records

{ifelse(nrow(entity$resources$updated_records)>0, paste0('For download see attached file: ', entity$resources$updated_records_filename), '')}

{ifelse(nrow(entity$resources$updated_records)>0, entity$resources$updated_records_table, 'No updated records')}
	
Methodology used to detect geo-reference updates: Geo-references updates are cross-checked with the previous geo-references, based on a buffer tolerance of 0.1 decimal degree, ie. A geo-reference placemark which deviation from its location in the previous dataset is within the tolerance threshold is considered as unchanged.
	
Best regards,

The FIRMS stocks & fisheries map viewer workflow
")),
	footer = md(glue::glue("
Report automatically generated on {Sys.time()} by the FIRMS map viewer workflow
"))
    )
 
	# Preview the message in the Viewer
	email
	
	#add attachments
	setwd("reports")
	files_to_attach <- list.files()
	for(file_to_attach in files_to_attach){
		content_type = mime::guess_type(file_to_attach)
		filename = basename(file_to_attach)
		expanded_path <- file_to_attach %>% path.expand() %>% normalizePath(mustWork = TRUE)
		attachment_list <- list(file_path = expanded_path, content_type = content_type, 
							disposition = "attachment", filename = filename)
		email$attachments <- c(email$attachments, list(attachment_list))
	}
	setwd("..")
	
	#SEND EMAIL
	
	# Sending email to a personal account
	# through manual specification of SMTP
	# credentials
	
	if(entity$resources$is_review){
	  email %>%
	    smtp_send(
	      from = c("FIRMS Stocks & Fisheries map viewer" = "firms.viewer@gmail.com"),
	      to = c(
	        "Aureliano Gentile" = "aureliano.gentile@fao.org",
	        "Kiran Viparthi" = "kiran.viparthi@fao.org"
	      ),
	      bcc = c(
	        "Emmanuel Blondel" = "emmanuel.blondel@fao.org"
	      ),
	      subject = sprintf("[REVIEW] FIRMS stocks & fisheries map viewer update (%s) - %s dataset", Sys.Date(), entity$resources$firmsDomainName),
	      credentials = SMTP
	    )
	}else{
	  email %>%
	   smtp_send(
	  from = c("FIRMS Stocks & Fisheries map viewer" = "firms.viewer@gmail.com"),
	  to = c("FIRMS Secretariat" = "firms-secretariat@fao.org"),
	  bcc = c(
	  	"emmanuel.blondel@fao.org","aureliano.gentile@fao.org", "bracken.vanniekerk@fao.org", "marc.taconet@fao.org",
	  	"kiran.viparthi@fao.org", "anton.ellenbroek@fao.org", "yann.laurent@fao.org", "james.geehan@fao.org"
	  ),
	  subject = sprintf("FIRMS stocks & fisheries map viewer update (%s) - %s dataset", Sys.Date(), entity$resources$firmsDomainName),
	  credentials = creds_key("firms.viewer@gmail.com")
	  )
	}

}