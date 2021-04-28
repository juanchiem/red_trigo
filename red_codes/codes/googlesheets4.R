library(googlesheets4)

read_sheet("1U6Cf_qEOhiR9AZqTqS3mbMF3zt2db48ZP5v3rkrAEJY")

sheets_get()
as_sheets_id(ss)

sheets_get("1hC8TSs3hJVMEDTlO_yulRRHL_FlI7wLV2XAdm45jGZw") 
read_sheet("1hC8TSs3hJVMEDTlO_yulRRHL_FlI7wLV2XAdm45jGZw", sheet="neco")


sheets_deauth()
sheets_get("https://docs.google.com/spreadsheets/d/1hC8TSs3hJVMEDTlO_yulRRHL_FlI7wLV2XAdm45jGZw/edit?usp=sharing")

da <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1hC8TSs3hJVMEDTlO_yulRRHL_FlI7wLV2XAdm45jGZw/edit?usp=sharing", 
                                sheet="neco")

library(googledrive)

x <- drive_get("Red_cosecha")

drive_share(x, role = "reader", type = "anyone")

da <- googlesheets4::read_sheet(x,sheet="neco")

library(googlesheets4)
sheets_auth(email = "edwardsmolina@gmail.com")

# put the googlesheets4 code you want to reproduce below here
# the following is just an example, replace it with your own code
ssid <- "1hC8TSs3hJVMEDTlO_yulRRHL_FlI7wLV2XAdm45jGZw"
sheets_get(ssid)


sheets_examples()

sheets_example("mini-gap") %>% 
  read_sheet()
