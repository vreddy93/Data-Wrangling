library(tidyr)
library(dplyr)

#load data

data<- read.csv("~/Desktop/R/Data Wrangling/Projects/Electronics Store/refine_original.csv")
refine<-tbl_df(data)

#Clean up brand names

p<-agrep(pattern = "philips", x = refine$company, ignore.case = TRUE, value = FALSE, 
               max.distance = 3)
a<-agrep(pattern = "akzo", x = refine$company, ignore.case = TRUE, value = FALSE, 
              max.distance = 3)
v<-agrep(pattern = "van houten", x = refine$company, ignore.case = TRUE, value = FALSE, 
              max.distance = 3)
u<-agrep(pattern = "unilever", x = refine$company, ignore.case = TRUE, value = FALSE, 
              max.distance = 3)
refine$company[p]<-"philips"
refine$company[a]<-"akzo"
refine$company[v]<-"van houten"
refine$company[u]<-"unilever"

#Separate Product Code and Product Number

refine<-separate(refine,Product.code...number,c("product_code","product_number"),"-")

#Add Product Categories 
#p = Smartphone; v = TV; x = Laptop; q = Tablet

refine<- mutate(refine, product_category =ifelse(grepl("p",product_code),"Smartphone",
                  ifelse(grepl("v",product_code),"TV",
                  ifelse(grepl("x",product_code),"Laptop",
                  ifelse(grepl("q",product_code),"Tablet" ,NA)))))

# New Column full_address for geocoding

refine<-unite(refine,"full_address",address,city,country,sep=",")

#Create dummy variables for company and product category

refine <- refine %>% 
  mutate(company_philips = ifelse(grepl("philips",company),1,0)) %>% 
  mutate(company_akzo = ifelse(grepl("akzo",company),1,0)) %>%
  mutate(company_van_houten = ifelse(grepl("van_houten",company),1,0)) %>%
  mutate(company_unilever = ifelse(grepl("unilever",company),1,0)) %>%
  
  mutate(product_smartphone = ifelse(grepl("Smartphone",product_category),1,0)) %>% 
  mutate(product_tv= ifelse(grepl("TV",product_category),1,0)) %>% 
  mutate(product_laptop = ifelse(grepl("Laptop",product_category),1,0)) %>% 
  mutate(product_tablet = ifelse(grepl("Tablet",product_category),1,0))

#Write Cleaned up data

write.csv(refine,"~/Desktop/R/Data Wrangling/Projects/Electronics Store/refine_clean.csv")
                  
                
                            


