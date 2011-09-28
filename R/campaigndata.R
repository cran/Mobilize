

#' Wrapper for oh.campaign.read
#' @return List of campaigns and prompts
#' @param ... arguments passed on to oh.campaign.read
#' @export
campaigndata <- function(...){
	
	#remainig arguments should contain read data.
	campaigns <- oh.campaign.read(output="long", ...);
	
	if(length(campaigns$data) == 0){
		stop("User does not have access to any campaigns.")
	}
	
	for(i in 1:length(campaigns$data)){
		doc <- xmlTreeParse(campaigns$data[[i]]$xml, useInternalNodes=T);
		campaigns$data[[i]]$xml <- NULL;
		campaigns$data[[i]]$promptIDs <- unlist(xpathApply(doc, "//prompt/id",xmlValue));
		campaigns$data[[i]]$promptTypes <- unlist(xpathApply(doc, "//prompt/promptType",xmlValue));
		campaigns$data[[i]]$surveys <- unlist(xpathApply(doc, "//survey/id",xmlValue));
		campaigns$data[[i]]$description <- NULL;
	}
	
	return(campaigns);	
}
