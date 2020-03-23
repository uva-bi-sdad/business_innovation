
project_folder <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/"

ndc_sec_ref <- readxl::read_excel(paste0(project_folder, "working/NDC_SEC_CompNames/final_ndc_sec_companies.xlsx"), sheet = "finalset") %>% select(-12,-13, -14)
colnames(ndc_sec_ref) <- dataplumbr::name.standard_col_names(colnames(ndc_sec_ref))
ndc_sec_ref <- ndc_sec_ref %>% filter(is.na(dupe)) %>% select(-dupe)

refciks <- unique(ndc_sec_ref$cik)

paths_file <- paste0(project_folder, "original/edgar_filings/ALL_SEC_files.txt")
file_headers <- readr::read_tsv(paths_file, col_names = FALSE)

patt <- paste0("^(", paste0(refciks, collapse = "|"), ")" )

ndc_file_headers <- file_headers[str_detect(file_headers$X1, pattern = patt),][[1]]
paths <- paste0(project_folder, "original/edgar_filings/Edgar_filings_folders/", file_headers$X1)
file_names <- unique(list.files(paths, full.names = TRUE))

ndc_paths <- paste0(project_folder, "original/edgar_filings/Edgar_filings_folders/", ndc_file_headers)
ndc_file_names <- unique(list.files(ndc_paths, full.names = TRUE))
ndc_sec_ref %>% filter(family == "glaxosmithkline") %>% .$cik %>% unique
ndc_sec_ref %>% filter(family == "teva") %>% .$cik %>% unique
test <- ndc_file_names[str_detect(ndc_file_names, "818686")]

test[1]

unclean <- read_file(test[1])
xml_string <- unclean

remove_xbrl <- function(xml_string) {
  no_ns <- gsub("\\n", " ", xml_string)
  #browser()
  find_str <- paste0("(<|&#60;|&#x3c;|&lt;|\u003C)", "XBRL", "(>|&#62;|&#x3e;|&gt;|\u003E)", " ?")
  #find_str <- "XBRL"
  search_str <- paste0("<XBRL> ?",  ".*?</XBRL>")
  found <- as.data.table(stringr::str_locate_all(no_ns, find_str))
  #testing <- substr(no_ns, start = (locs$start[1] - 1), stop = (locs$end[1] + 1))

  for (i in 1:nrow(found)) {
    locs <- as.data.table(stringr::str_locate(no_ns, search_str))
    st <- locs[1, start] - 1
    en <- locs[1, end] + 1

    ifelse(is.na(locs$start) == TRUE & is.na(locs$end) == TRUE, no_ns,
           no_ns <- paste0(substr(no_ns, 1, st), substr(no_ns, en, nchar(no_ns))) )

    # if (is.na(locs$start) == TRUE & is.na(locs$end) == TRUE) {no_ns}
    # else {
    #   no_ns <- paste0(substr(no_ns, 1, st), substr(no_ns, en, nchar(no_ns)))
    #   no_ns_list[i] <- substr(no_ns, start = (locs$start[1] - 1), stop = (locs$end[1] + 1))
    # }

  }
  no_ns
}

xml_string_ <- xml_string %>%
  remove_doc_types() %>%
  gsub(pattern = "&#60;|&#x3c;|&lt;|\u003C", replacement = "<") %>%
  gsub(pattern = "&#62;|&#x3e;|&gt;|\u003E", replacement = ">") %>%
  gsub(pattern = paste0("<tr> ?",  ".*?</tr>"), replacement = " ") %>%
  gsub(pattern = paste0("<MyReports> ?",  ".*?</MyReports>"), replacement = " ") %>%
  read_html() %>%
  html_text()


no_ns_noxbrl_repcarrots <- gsub(x = xml_string_, pattern = "&#60;|&#x3c;|&lt;|\u003C", replacement = "<")
no_ns_noxbrl_repcarrots <- gsub(x = no_ns_noxbrl_repcarrots, pattern = "&#62;|&#x3e;|&gt;|\u003E", replacement = ">")
#no_ns_noxbrl_repcarrots <- gsub(x = no_ns_noxbrl_repcarrots, pattern = paste0("<XBRL> ?",  ".*?</XBRL>"), replacement = " ")
no_ns_noxbrl_repcarrots <- gsub(x = no_ns_noxbrl_repcarrots, pattern = paste0("<tr> ?",  ".*?</tr>"), replacement = " ")
no_ns_noxbrl_repcarrots <- gsub(x = no_ns_noxbrl_repcarrots, pattern = paste0("<MyReports> ?",  ".*?</MyReports>"), replacement = " ")

no_ns_noxbrl <- read_html(no_ns_noxbrl_repcarrots)
#no_ns_noxbrl_coll <- no_ns_noxbrl %>% html_text_collapse()
no_ns_noxbrl_reg <- no_ns_noxbrl %>% html_text()

# install.packages("XBRL")
library(XBRL)

no_ns_list[[2]]

new_unclean <- remove_xbrl(unclean)

xbrl.vars <- xbrlDoAll(no_ns_list[[2]])

xbrl.vars <- xbrlDoAll(unclean)

unclean
no_ns
no_ns_list[[2]]

inst <- "https://www.sec.gov/Archives/edgar/data/818686/000119312513050510/teva-20121231.xml"
options(stringsAsFactors = FALSE)
xbrl.vars <- xbrlDoAll(inst)

xbrl.vars$label

xbrl.vars$fact

str(xbrl.vars, max.level = 1)



weirdstring2 <- str_replace_all(weirdstring, pattern =  "&lt;", replacement =  "<")
weirdstring2 <- str_replace_all(weirdstring2, pattern =  "&gt;", replacement =  ">")

weirdstring3 <- read_html(weirdstring2)

cleaned <- weirdstring3 %>%
  remove_doc_types() %>%
  iconv(to = "ASCII") %>%
  read_html() %>%
  xml_find_all(".//body") %>%
  html_text_collapse() %>%
  textclean::replace_non_ascii2(replacement = " ")

install.packages("textutils")

unclean_decode_html <- textutils::HTMLdecode(unclean)

unclean_decode_html_cleaned <- unclean_decode_html %>%
  remove_doc_types() %>%
  iconv(to = "ASCII") %>%
  read_html() %>%
  xml_find_all(".//body") %>%
  html_text_collapse() %>%
  textclean::replace_non_ascii2(replacement = " ")


words <- unlist(str_split(str_replace_all(unlist(str_split(xml_string_, "\\s", simplify = FALSE)), pattern = "[[:punct:]]", replacement = " "), "\\s"))
words2 <- words[dataplumbr::var.is_blank(str_squish(words)) == FALSE & nchar(words) > 3 & grepl(x = words, pattern =  "[[:alpha:]]") == TRUE]
words_cc <- words2[str_detect(words2, "[a-z][A-Z][a-z]") & nchar(words2)> 25]
words_ncc <- words2[str_detect(words2, "[a-z][A-Z][a-z]") == FALSE & nchar(words2)<= 25]

words_cc <- str_squish(unlist(str_split(words_cc, "(?=[A-Z])", simplify = FALSE)))
words_cc <- words_cc[dataplumbr::var.is_blank(str_squish(words_cc)) == FALSE & nchar(words_cc) > 3 & grepl(x = words_cc, pattern =  "[[:alpha:]]") == TRUE]

words <- c(words_ncc, words_cc)
eng <- hunspell_check(words)
init_caps <- grepl("^[[:upper:]]", words)
brand <- str_extract(words, "®|™")

output1 <- tibble::tibble(
  # "Company" = company,
  # "Month" = month,
  # "Year" = year,
  "Words" = words,
  "English" = eng,
  "Capitals" = init_caps,
  "Brand" = brand
)



weirdstring<- '&lt;/td&gt;
&lt;/tr&gt;
&lt;/table&gt;
&lt;p style="MARGIN-TOP: 12px; TEXT-INDENT: 4%; MARGIN-BOTTOM: 0px"&gt;
&lt;font style="FONT-FAMILY: Times New Roman" size="2"&gt;In its
analyses, the Company uses prescription data purchased from a
third-party data provider to develop estimates of historical
inventory channel pull-through. The Company utilizes an internal
analysis to compare historical net product shipments to estimated
historical prescriptions written. Based on that analysis,
management develops an estimate of the quantity of product in the
channel which may be subject to various rebate, chargeback and
product return exposures. At least quarterly for each product line,
the Company prepares an internal estimate of ending inventory units
in the distribution channel by adding estimated inventory in the
channel at the beginning of the period, plus net product shipments
for the period, less estimated prescriptions written for the
period. Based on that analysis, the Company develops an estimate of
the quantity of product in the channel that might be subject to
various rebate, chargeback and product return exposures. This is
done for each product line by applying a rate of historical
activity for rebates, chargebacks and product returns, adjusted for
relevant quantitative and qualitative factors discussed above, to
the potential exposed product estimated to be in the distribution
channel. The Company regularly adjusts internal forecasts that are
utilized to calculate the estimated number of months in the channel
based on input from members of the Company&amp;#x2019;s sales, marketing
and operations groups. The adjusted forecasts take into account
numerous factors including, but not limited to, new product
introductions, direct communication with customers and potential
product expiry issues. Adjustments to estimates are recorded in the
period when significant events or changes in trends are
identified.&lt;/font&gt;&lt;/p&gt;
&lt;p style="MARGIN-TOP: 12px; TEXT-INDENT: 4%; MARGIN-BOTTOM: 0px"&gt;
&lt;font style="FONT-FAMILY: Times New Roman" size="2"&gt;The Company
periodically offers promotional discounts to the Company&amp;#x2019;s
existing customer base. These discounts are calculated as a
percentage of the current published list price and are treated as
off-invoice allowances. Accordingly, the discounts are recorded as
a reduction of revenue in the period that the program is offered.
In addition to promotional discounts, at the time that the Company
implements a price increase, it generally offers its existing
customer base an opportunity to purchase a limited quantity of
product at the previous list price. Shipments resulting from these
programs generally are not in excess of ordinary levels, therefore,
the Company recognizes the related revenue upon shipment and
includes the shipments in estimating various product related
allowances. In the event the Company determines that these
shipments represent purchases of inventory in excess of ordinary
levels for a given wholesaler, the potential impact on product
returns exposure would be specifically evaluated and reflected as a
reduction in revenue at the time of such shipments.&lt;/font&gt;&lt;/p&gt;
&lt;p style="MARGIN-TOP: 12px; TEXT-INDENT: 4%; MARGIN-BOTTOM: 0px"&gt;
&lt;font style="FONT-FAMILY: Times New Roman" size="2"&gt;Allowances for
estimated rebates, chargebacks and promotional programs were $103.8
million and $69.2 million as of December&amp;#xA0;31, 2012 and 2011,
respectively. These allowances reflect an estimate of the
Company&amp;#x2019;s liability for items such as rebates due to various
governmental organizations under the Medicare/Medicaid regulations,
rebates due to managed care organizations under specific contracts
and chargebacks due to various organizations purchasing products
through federal contracts and/or group purchasing agreements. The
Company estimates its liability for rebates and chargebacks at each
reporting period based on a methodology of applying quantitative
and qualitative assumptions discussed above. Due to the
subjectivity of the Company&amp;#x2019;s accrual estimates for rebates
and chargebacks, the Company prepares various sensitivity analyses
to ensure the Company&amp;#x2019;s final estimate is within a reasonable
range as well as review prior period activity to ensure that the
Company&amp;#x2019;s methodology continues to be appropriate.&lt;/font&gt;&lt;/p&gt;
&lt;p style="MARGIN-TOP: 12px; TEXT-INDENT: 4%; MARGIN-BOTTOM: 0px"&gt;
&lt;font style="FONT-FAMILY: Times New Roman" size="2"&gt;Allowances for
product returns were $36.4 million and $28.7 million as of
December&amp;#xA0;31, 2012 and 2011, respectively. These allowances
reflect an estimate of the Company&amp;#x2019;s liability for products
that may be returned by the original purchaser in accordance with
the Company&amp;#x2019;s stated return policy. The Company estimates its
liability for product returns at each reporting period based on
historical return rates, estimated inventory in the channel and the
other factors discussed above. Due to the subjectivity of the
Company&amp;#x2019;s accrual estimates for product returns, the Company
prepares various sensitivity analyses and also reviews prior period
activity to ensure that the Company&amp;#x2019;s methodology is still
reasonable.&lt;/font&gt;&lt;/p&gt;
&lt;p style="MARGIN-TOP: 12px; TEXT-INDENT: 4%; MARGIN-BOTTOM: 0px"&gt;
&lt;font style="FONT-FAMILY: Times New Roman" size="2"&gt;The
Company&amp;#x2019;s provision for revenue-reducing items such as
rebates, chargebacks, and product returns as a percentage of gross
product revenue in the years ended December&amp;#xA0;31, 2012, 2011 and
2010 was 15.7%, 14.6% and 14.9% for rebates, chargebacks and
discounts and was 2.3%, 3.9% and 2.9% for product returns,
respectively.&lt;/font&gt;&lt;/p&gt;
&lt;p style="MARGIN-TOP: 12px; TEXT-INDENT: 4%; MARGIN-BOTTOM: 0px"&gt;
&lt;font style="FONT-FAMILY: Times New Roman" size="2"&gt;During the
second quarter of 2010 the Company recognized product revenue
related to initial shipments to wholesalers of Xifaxan 550mg, which
the FDA approved on March&amp;#xA0;24, 2010 for reduction in risk of
overt hepatic encephalopathy, or HE, recurrence in patients 18
years of age or older, and launched to physicians in May 2010.
Based on our historical experience with Xifaxan 200mg, which the
Company distributes through the same distribution channels and is
prescribed by the same physicians as Xifaxan 550mg, we have the
ability to estimate returns for Xifaxan 550mg and therefore
recognized revenue upon shipment to the wholesalers.&lt;/font&gt;&lt;/p&gt;
&lt;p style="MARGIN-TOP: 12px; TEXT-INDENT: 4%; MARGIN-BOTTOM: 0px"&gt;
&lt;font style="FONT-FAMILY: Times New Roman" size="2"&gt;During the
second quarter of 2011, the Company began recognizing product
revenue related to shipments to wholesalers of Relistor, which the
Company acquired from Progenics Pharmaceuticals, Inc. in February
2011. Based on historical experience with Relistor obtained from
Progenics, and historical experience with the Company&amp;#x2019;s
products, specifically Xifaxan 200mg, Xifaxan 500mg and Apriso,
which the Company distributes through the same distribution
channels and are prescribed by the same physicians as Relistor,
management has the ability to estimate returns for Relistor and
therefore recognized revenue upon shipment to the
wholesalers.&lt;/font&gt;&lt;/p&gt;
&lt;p style="MARGIN-TOP: 12px; TEXT-INDENT: 4%; MARGIN-BOTTOM: 0px"&gt;
&lt;font style="FONT-FAMILY: Times New Roman" size="2"&gt;In December
2011, the Company acquired an exclusive worldwide license to
Solesta and Deflux with the completion of its acquisition of Oceana
Therapeutics, Inc. Solesta and Deflux are medical devices that the
Company sells to specialty distributors who then sell the products
to end users, primarily hospitals, surgical centers and physicians.
The specialty distributors generally do not purchase these products
until an end user is identified. Based on historical experience
with these products obtained from Oceana, and historical experience
with the Company&amp;#x2019;s products, specifically Xifaxan 200mg,
Xifaxan 550mg and Apriso, which are prescribed by the same
physicians as Solesta, management has the ability to estimate
returns for Solesta and Deflux and therefore recognized revenue
upon shipment to the specialty distributors.&lt;/font&gt;&lt;/p&gt;
&lt;/div&gt;</us-gaap:RevenueRecognitionPolicyTextBlock>
'
