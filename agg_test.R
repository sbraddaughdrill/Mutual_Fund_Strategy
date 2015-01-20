mflink1_no_na <- mflink1[!(is.na(mflink1[,"crsp_fundno"])),]

monthly_full0 <- merge(monthly_tna_ret_nav2, mflink1_no_na, by.x=c("crsp_fundno"), by.y=c("crsp_fundno"), 
                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

monthly_full0 <- subset(monthly_full0,select=-c(caldt))

monthly_full1 <- merge(monthly_full0, fund_fees_month, by.x=c("crsp_fundno","yr","month"), by.y=c("crsp_fundno","yr","month"), 
                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

monthly_full1 <- subset(monthly_full1,select=-c(begdt,enddt,fiscal_yearend))

monthly_full2 <- merge(monthly_full1, fund_style_month, by.x=c("crsp_fundno","yr","month"), by.y=c("crsp_fundno","yr","month"), 
                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

monthly_full2 <- subset(monthly_full2,select=-c(begdt,enddt))

monthly_full <-monthly_full2[c("wficn","crsp_fundno","yr","month","mnav","mtna","mret","actual_12b1","max_12b1","exp_ratio","mgmt_fee",
                               "turn_ratio","crsp_obj_cd","accrual_fund","sales_restrict","lipper_class","lipper_class_name",
                               "lipper_obj_cd","lipper_obj_name","lipper_asset_cd","lipper_tax_cd")]

monthly_full <- monthly_full[order(monthly_full[,"wficn"], 
                                   monthly_full[,"crsp_fundno"], 
                                   monthly_full[,"yr"],
                                   monthly_full[,"month"]),]

rm(monthly_full0,monthly_full1,monthly_full2)
capture.output(gc(),file='NUL')


####################


monthly_full_no_tna_na <- monthly_full[!(is.na(monthly_full[,"mtna"])),]

monthly_full_no_na1 <- monthly_full_no_tna_na
monthly_full_no_na1 <- monthly_full_no_na1[!(is.na(monthly_full_no_na1[,"mnav"])),]
monthly_full_no_na1 <- monthly_full_no_na1[!(is.na(monthly_full_no_na1[,"mret"])),]

monthly_full_no_na2 <- monthly_full_no_tna_na
monthly_full_no_na2 <- monthly_full_no_na2[!(is.na(monthly_full_no_na2[,"actual_12b1"])),]
#monthly_full_no_na2 <- monthly_full_no_na2[!(is.na(monthly_full_no_na2[,"max_12b1"])),]
monthly_full_no_na2 <- monthly_full_no_na2[!(is.na(monthly_full_no_na2[,"exp_ratio"])),]
monthly_full_no_na2 <- monthly_full_no_na2[!(is.na(monthly_full_no_na2[,"mgmt_fee"])),]
monthly_full_no_na2 <- monthly_full_no_na2[!(is.na(monthly_full_no_na2[,"turn_ratio"])),]

monthly_full_no_na3 <- monthly_full_no_tna_na
monthly_full_no_na3 <- monthly_full_no_na3[!(is.na(monthly_full_no_na3[,"crsp_obj_cd"])),]
monthly_full_no_na3 <- monthly_full_no_na3[!(is.na(monthly_full_no_na3[,"accrual_fund"])),]
monthly_full_no_na3 <- monthly_full_no_na3[!(is.na(monthly_full_no_na3[,"sales_restrict"])),]
monthly_full_no_na3 <- monthly_full_no_na3[!(is.na(monthly_full_no_na3[,"lipper_class"])),]
monthly_full_no_na3 <- monthly_full_no_na3[!(is.na(monthly_full_no_na3[,"lipper_class_name"])),]
monthly_full_no_na3 <- monthly_full_no_na3[!(is.na(monthly_full_no_na3[,"lipper_obj_cd"])),]
monthly_full_no_na3 <- monthly_full_no_na3[!(is.na(monthly_full_no_na3[,"lipper_obj_name"])),]
monthly_full_no_na3 <- monthly_full_no_na3[!(is.na(monthly_full_no_na3[,"lipper_asset_cd"])),]
monthly_full_no_na3 <- monthly_full_no_na3[!(is.na(monthly_full_no_na3[,"lipper_tax_cd"])),]













