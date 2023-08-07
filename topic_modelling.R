# topic modelling

# Data acquisition
## import data
library(tm)
sport_news <- Corpus(DirSource("Task1", encoding = "UTF-8"))
# Loop through each document in the Corpus
for (i in 1:length(sport_news)) {
  doc <- sport_news[[i]]  # Get the current document
  
  # Extract the content of the document
  content <- content(doc)
  
  # Extract the title (before the first occurrence of "\n\n")
  title <- substr(content, 1, regexpr("\n\n", content) - 1)
  
  # Print the title
  print(title)
}
writeLines(as.character(sport_news[[1]]))



## Data Acquisition and Data Pre-processing
### transform text to lowercase
sport_news <- tm_map(sport_news, content_transformer(tolower))
writeLines(as.character(sport_news[[1]]))
### remove punctuation
sport_news <- tm_map(sport_news, removePunctuation)
writeLines(as.character(sport_news[[1]]))
### remove numbers
sport_news <- tm_map(sport_news, removeNumbers)
writeLines(as.character(sport_news[[1]]))
### remove stopwords
sport_news <- tm_map(sport_news, removeWords, stopwords("english"))
writeLines(as.character(sport_news[[1]]))
### remove extra whitespaces
sport_news <- tm_map(sport_news, stripWhitespace)
writeLines(as.character(sport_news[[1]]))
### stemming
sport_news <- tm_map(sport_news,stemDocument)
writeLines(as.character(sport_news[[1]]))
### export the corpus data to look for other words that need to remove
### writeCorpus(sport_news, path = "C:/Users/User/Downloads/output_sport")
### change words (names, places, countries, and etc)
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bsingapor open\\b", replacement = "open")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bkuala lumpur\\b", replacement = "kualalumpur")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\btanmthinaah\\b", replacement = "pearlythinaah")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bbaek ha nale hee\\b", replacement = "baekhanalesohee")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\baxiata arena\\b", replacement = "axiataarena")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bbirmingham commonwealth game champion\\b", 
                     replacement = "birminghamcommonwealthgamechampion")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\brena miyauraayako sakuramoto\\b", 
                     replacement = "renamiyauraayakosakuramoto")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\brin iwanagaki nakanishi\\b", 
                     replacement = "riniwanagakinakanishi")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bchong wei\\b", replacement = "leechongwei")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\blee chong wei\\b", replacement = "leechongwei")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\btaufik hidayat\\b", replacement = "taufikhidayat")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bbadminton hall fame\\b", 
                     replacement = "badmintonhalloffame")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\blin dan\\b", replacement = "lindan")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bbadminton world feder bwf\\b", replacement = "bwf")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\btaufiq\\b", replacement = "taufikhidayat")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bathen olymp\\b", replacement = "olympic")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bworld championship\\b", 
                     replacement = "worldchampionship")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bsea game\\b", replacement = "seagame")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bsze fei\\b", replacement = "gohszefei")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bgoh sze fei\\b", replacement = "gohszefei")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bnur izzuddin rumsani\\b", 
                     replacement = "nurizzuddinrumsani")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bizzuddin\\b", replacement = "nurizzuddinrumsani")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bpari olymp\\b", replacement = "olympic")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bacademi badminton malaysia bukit kiara\\b", 
                     replacement = "abm")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bgerman open\\b", replacement = "open")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bnur izzuddin\\b", 
                     replacement = "nurizzuddinrumsani")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bhaikal nazri\\b", replacement = "haikalnazri")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bwan arif wan junaidi\\b", 
                     replacement = "wanarifwanjunaidi")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bwan arif\\b", replacement = "wanarifwanjunaidi")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\byap roy king\\b", replacement = "yaproyking")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bchoong hon jian\\b", replacement = "choonghonjian")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bhon jian\\b", replacement = "choonghonjian")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bsze feiizzuddin\\b", replacement = "szefeiizzuddin")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\baaron chiasoh wooi yik\\b", 
                     replacement = "aaronchiasohwooiyik")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bong yew sinteo ee yi\\b", 
                     replacement = "ongyewsinteoeeyi")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bman wei chongte kai wun\\b", 
                     replacement = "manweichongtekaiwun")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\btze yong\\b", replacement = "ngtzeyong")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bng tze yong\\b", replacement = "ngtzeyong")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bhong kong\\b", replacement = "hongkong")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\blee cheuk yiu\\b", replacement = "leecheukyiu")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bthailand open\\b", replacement = "open")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bcheuk yiu\\b", replacement = "leecheukyiu")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\basia championship\\b", replacement = "asiachampionship")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bchou tien chen\\b", replacement = "choutienchen")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bindia open\\b", replacement = "open")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bindia thoma cup\\b", replacement = "thomascup")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\blakhsya sen\\b", replacement = "lakhsyasen")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bjun hao\\b", replacement = "leongjunhao")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bphnom penh sea game\\b", replacement = "seagame")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\btaiwan open\\b", replacement = "open")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\btien chen\\b", replacement = "choutienchen")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmalaysia master\\b", replacement = "malaysiamaster")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bchou tien chen\\b", replacement = "choutienchen")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\btaiwanes\\b", replacement = "taiwan")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bswiss open\\b", replacement = "open")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bengland\\b", replacement = "allengland")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bwong choonh hann\\b", replacement = "wongchoonhhann")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmagnus johannesen\\b", replacement = "magnusjohannesen")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bleong jun hao\\b", replacement = "leongjunhao")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bchristo popov\\b", replacement = "christopopov")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bkieran georg\\b", replacement = "kierangeorge")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bkieran\\b", replacement = "kierangeorge")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bweng hong yang\\b", replacement = "wenghongyang")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bshi yu qi\\b", replacement = "shiyuqi")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bgoh soon huatshevon lai jemi\\b", 
                     replacement = "gohsoonhuatshevonlaijemi")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bchen tang jietoh ee wei\\b", 
                     replacement = "chentangjietoheewei")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bchan peng sooncheah yee see\\b", 
                     replacement = "chanpengsooncheahyeesee")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bsoon huatshevon\\b", replacement = "soonhuatshevon")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\btang chun mants ying suet\\b", 
                     replacement = "tangchunmantsyingsuet")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\btang jieee wei\\b", replacement = "tangjieeewei")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bfeng yan zhehuang dong ping\\b", 
                     replacement = "fengyanzhehuangdongping")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\byuki kanekomisaki matsutomo\\b", 
                     replacement = "yukikanekomisakimatsutomo")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bpeng soonye see\\b", replacement = "pengsoonyesee")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bkim won hojeong na eun\\b", 
                     replacement = "kimwonhojeongnaeun")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bsouth korea\\b", replacement = "southkorea")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bzii jia\\b", replacement = "leeziijia")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bolymp\\b", replacement = "olympic")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\blee zii jia\\b", replacement = "leeziijia")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bgrant road gold\\b", replacement = "grantroadgold")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bwong tat meng\\b", replacement = "wongtatmeng")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bliew daren\\b", replacement = "liewdaren")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bviktor axelsenh\\b", replacement = "viktoraxelsenh")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmalaysia chef de mission\\b", 
                     replacement = "malaysiachefdemission")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bdatuk hamidin mohd amin\\b", 
                     replacement = "datukhamidinmohdamin")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bhamidin\\b", replacement = "datukhamidinmohdamin")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\btat meng\\b", replacement = "wongtatmeng")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bacademi badminton malaysia abm bukit kiara\\b", 
                     replacement = "abm")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bindonesian open\\b", replacement = "open")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bpari game\\b", replacement = "parisgame")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bjohn beasley\\b", replacement = "johnbeasley")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmalaysian nation cycl feder mncf\\b", 
                     replacement = "mncf")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bdatuk abu samah abd wahab\\b", 
                     replacement = "datukabusamahabdwahab")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bayer keroh session court\\b", 
                     replacement = "sessioncourt")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bjudg elesabet paya wan\\b", 
                     replacement = "elesabetpayawan")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmalaysian anticorrupt commiss macc\\b", 
                     replacement = "macc")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bdeputi public prosecutor mahadi abdul jumaat\\b", 
                     replacement = "mahadiabduljumaat")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\babu samah\\b", 
                     replacement = "datukabusamahabdwahab")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bazrul zulkifli stork\\b", 
                     replacement = "azrulzulkiflistork")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bnorazman abu samah\\b", 
                     replacement = "norazmanabusamah")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\basean cycl feder acf\\b", replacement = "acf")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bdatuk amarjit singh gill\\b", 
                     replacement = "datukamarjitsinghgill")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bsyarikat harapan baiduri sdn bhd\\b", 
                     replacement = "syarikatharapanbaiduri")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bsyarikat keluarga haji wahab hassan sdn bhd\\b", 
                     replacement = "syarikatkeluargahajiwahabhassan")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\broglic\\b", replacement = "primozroglic")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\btour de franc\\b", replacement = "tourdefrance")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bprimoz roglic\\b", replacement = "primozroglic")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bgiro ditalia\\b", replacement = "giro")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bjumbo visma rider roglic\\b", 
                     replacement = "primozroglic")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bvuelta espana\\b", replacement = "vueltaespana")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bgeraint thoma\\b", replacement = "geraintthomas")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bslovenian\\b", replacement = "slovenia")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\btadej pogacar\\b", replacement = "tadejpogacar")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bla grand boucl\\b", replacement = "lagrandeboucle")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bthoma\\b", replacement = "geraintthomas")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\broman forum\\b", replacement = "deiforiimperiali")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bveteran ineo rider thoma\\b", replacement = "romanforum")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\btao geoghegan hart\\b", replacement = "geraintthomas")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bastana cavendish\\b", replacement = "astanacavendish")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bgrand tour\\b", replacement = "grandtour")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bcavendish\\b", replacement = "astanacavendish")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\blui leon sanchez\\b", replacement = "luileonsanchez")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmaxim bouet\\b", replacement = "maximebouet")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bcesar benedetti\\b", replacement = "cesarebenedetti")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\btom skujin\\b", replacement = "tomskujins")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bintern track cup seri\\b", 
                     replacement = "internationaltrackcupseries")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\basian track cycl championship acc\\b", replacement = "acc")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bazizulhasni awang\\b", replacement = "azizulhasniawang")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bfadhil zoni\\b", replacement = "fadhilzonis")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bamber yong ann tung\\b", replacement = "amberyonganntung")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bfadhil\\b", replacement = "fadhilzonis")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\btrack cup ii\\b", replacement = "internationaltrackcupseries")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bakmal nazimi jusena\\b", replacement = "akmalnazimijusena")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bshah firdaus sahrom\\b", replacement = "shahfirdaussahrom")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bnurul izzah izzati mohd asri\\b", 
                     replacement = "nurulizzahizzatimohdasri")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bamber\\b", replacement = "amberyonganntung")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bkang seojin\\b", replacement = "kangseojin")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bandrey chugay\\b", replacement = "andreychugay")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bintern track cup friday ii\\b", 
                     replacement = "internationaltrackcupseries")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\byong ann tung\\b", replacement = "amberyonganntung")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bann tung\\b", replacement = "amberyonganntung")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bintern track cup\\b", 
                     replacement = "internationaltrackcupseries")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bazizul\\b", replacement = "azizulhasniawang")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bsergey ponomaryov\\b", replacement = "sergeyponomaryov")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bponomaryov astana track team\\b", 
                     replacement = "sergeyponomaryov")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bakmal nazimi jusena\\b", replacement = "akmalnazimijusena")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bshah firdaus sahrom\\b", replacement = "shahfirdaussahrom")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bakmal\\b", replacement = "akmalnazimijusena")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bshah\\b", replacement = "shahfirdaussahrom")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bvalencia tan\\b", replacement = "valenciatan")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bizzah\\b", replacement = "nurulizzahizzatimohdasri")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmalaysian\\b", replacement = "malaysia")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bpocket rocketman\\b", replacement = "azizulhasniawang")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bpark ji hae\\b", replacement = "parkjihae")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bworld championship glasgow\\b", 
                     replacement = "glasgowworldchampionship")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bjunior asia cup\\b", replacement = "jac")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bkakamigahara japan\\b", replacement = "japan")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\blailin abu hassan\\b", replacement = "lailinabuhassan")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\blailin\\b", replacement = "lailinabuhassan")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bsultan qaboo stadium\\b", replacement = "sultanqaboosstadium")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\blim dohyun\\b", replacement = "limdohyun")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\badam ashraf johari\\b", replacement = "adamashrafjohari")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\byoo seungho\\b", replacement = "yooseungho")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bt perabu\\b", replacement = "tperabu")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmohammad khan\\b", replacement = "mohammadkhan")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\babdul rehman\\b", replacement = "abdulrehman")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\babdul shahid\\b", replacement = "abdulshahid")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bahmad arbaz\\b", replacement = "ahmadarbaz")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bdanish aiman khairil anuar\\b", 
                     replacement = "danishaimankhairilanuar")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bjunior asia cup jac\\b", replacement = "jac")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bamin rahim\\b", replacement = "aminrahim")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bshahmi irfan suhaimi\\b", replacement = "shahmieirfansuhaimi")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bpark geon woo\\b", replacement = "parkgeonwoo")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmughni ka\\b", replacement = "mughnikamal")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bamin\\b", replacement = "aminrahim")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bshahmi\\b", replacement = "shahmieirfansuhaimi")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\basian hockey feder ahf\\b", replacement = "ahf")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bgroup b\\b", replacement = "groupb")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bworld cup\\b", replacement = "juniorworldcup")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bjunior world cup\\b", 
                     replacement = "juniorworldcup")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bshafiq ikhmal daniel\\b", replacement = "shafiqikhmaldaniel")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bsyamim naim hamid\\b", replacement = "syamimnaimhamid")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmoham abdulah\\b", replacement = "mahamedabdulah")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bcambodian\\b", replacement = "cambodia")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bphnom penh\\b", replacement = "phnompenh")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmorodok techo nation stadium\\b", 
                     replacement = "morodoktechonationstadium")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bsoutheast asia\\b", replacement = "southeastasia")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bjunior world cup kuala lumpur\\b", 
                     replacement = "kljuniorworldcup")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bverstappen\\b", replacement = "maxverstappen")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmax verstappen\\b", replacement = "maxverstappen")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bsergio perez\\b", replacement = "sergioperez")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bred bull\\b", replacement = "redbull")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bgp practic\\b", replacement = "gp")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bformula one championship\\b", 
                     replacement = "formulaonechampionship")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bgrand prix\\b", replacement = "gp")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bcircuit de catalunya\\b", replacement = "circuitdecatalunya")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bperez\\b", replacement = "sergioperez")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bf\\b", replacement = "formulaonechampionship")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\balpin\\b", replacement = "alpine")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\besteban ocon\\b", replacement = "estebanocon")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bnyck de vri\\b", replacement = "nyckdevries")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bred bullown alphatauri\\b", replacement = "alphatauri")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bpierr gasli\\b", replacement = "pierregasly")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bfernando alonso\\b", replacement = "fernandoalonso")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\baston martin\\b", replacement = "astonmartin")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bcharl leclerc\\b", replacement = "charlesleclerc")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bcarlo sainz\\b", replacement = "carlosainz")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bgeorg russel\\b", replacement = "georgerussell")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bleclerc\\b", replacement = "charlesleclerc")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bsainz\\b", replacement = "carlosainz")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmexican\\b", replacement = "mexico")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\balonso\\b", replacement = "fernandoalonso")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\blanc stroll\\b", replacement = "lancestroll")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bgrand prix\\b", replacement = "gp")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bstroll\\b", replacement = "lancestroll")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\blawrenc stroll\\b", replacement = "lawrencestroll")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bformula one\\b", replacement = "formulaonechampionship")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bnico hamilton\\b", replacement = "nicohamilton")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\blewi hamilton\\b", replacement = "lewihamilton")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bhelmut marko\\b", replacement = "helmutmarko")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bspanish\\b", replacement = "spain")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bpari\\b", replacement = "paris")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bformula one champion\\b", 
                     replacement = "formulaonechampionship")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmax\\b", replacement = "maxverstappen")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bhungarian\\b", replacement = "hungary")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bvettel\\b", replacement = "sebastianvettel")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bsebastian vettel\\b", replacement = "sebastianvettel")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\btoro rosso now alphatauri\\b", replacement = "alphatauri")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bchristian horner\\b", replacement = "christianhorner")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bsebastian\\b", replacement = "sebastianvettel")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmont carlo\\b", replacement = "montecarlo")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\blas vega\\b", replacement = "lasvegas")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bdavid coulthard\\b", replacement = "davidcoulthard")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmclarenmerced\\b", replacement = "mclarenmercedes")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmercedes\\b", replacement = "mclarenmercedes")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmclaren\\b", replacement = "mclarenmercedes")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bcoulthard\\b", replacement = "davidcoulthard")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bconor mcgregor\\b", replacement = "conormcgregor")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmichael dougla\\b", replacement = "michaeldouglas")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bpari st germain\\b", replacement = "parisstgermain")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bkevin harrington\\b", replacement = "kevinharrington")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bharrington\\b", replacement = "kevinharrington")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bhamilton\\b", replacement = "lewishamilton")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\brussel\\b", replacement = "georgerussell")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bsvitolina\\b", replacement = "elinasvitolina")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bfrench open\\b", replacement = "open")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\belina svitolina\\b", replacement = "elinasvitolina")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\banna blinkova\\b", replacement = "annablinkova")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bgrand slam\\b", replacement = "grandslam")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bblinkova\\b", replacement = "annablinkova")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bcarolin garcia\\b", replacement = "carolingarcia")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bgael monfil\\b", replacement = "gaelmonfil")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bdaria kasatkina\\b", replacement = "dariakasatkina")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bsonego\\b", replacement = "lorenzosonego")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\brublev\\b", replacement = "andreyrublev")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bitalian\\b", replacement = "italy")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\blorenzo sonego\\b", replacement = "lorenzosonego")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bandrey rublev\\b", replacement = "andreyrublev")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bcourt suzann lenglen\\b", replacement = "courtsuzannelenglen")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bsonego\\b", replacement = "lorenzosonego")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bkaren khachanov\\b", replacement = "karenkhachanov")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bnovak djokov\\b", replacement = "novakdjokovic")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\brussian\\b", replacement = "russia")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bslam\\b", replacement = "grandslam")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmont carlo master\\b", replacement = "montecarlomaster")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bsabalenka\\b", replacement = "arynasabalenka")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bdjokov\\b", replacement = "novakdjokovic")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\baryna sabalenka\\b", replacement = "arynasabalenka")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bbelarusia\\b", replacement = "belaruse")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\biryna shymanovich\\b", replacement = "irynashymanovich")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\baustralian open\\b", replacement = "open")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmarta kostyuk\\b", replacement = "martakostyuk")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bkamilla rakhimova\\b", replacement = "kamillarakhimova")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\balejandro davidovich fokina\\b", 
                     replacement = "alejandrodavidovichfokina")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bbelgradeborn\\b", replacement = "belgrade")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\baleksandar kovacev\\b", replacement = "aleksandarkovacev")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmarton fucsov\\b", replacement = "martonfucsov")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bspanish\\b", replacement = "spain")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bdavidovich fokina\\b", 
                     replacement = "alejandrodavidovichfokina")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bcarlo alcaraz\\b", replacement = "carloalcaraz")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bcanadian\\b", replacement = "canada")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bdeni shapovalov\\b", replacement = "denishapovalov")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bus open\\b", replacement = "open")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bstefano tsitsipa\\b", replacement = "stefanotsitsipa")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bdiego schwartzman\\b", replacement = "diegoschwartzman")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bcameron norri\\b", replacement = "cameronnorrie")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\blorenzo musetti\\b", replacement = "lorenzomusetti")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bitali\\b", replacement = "italy")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\badriano panatta\\b", replacement = "adrianopanatta")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bjannik sinner\\b", replacement = "janniksinner")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\baltmaier\\b", replacement = "danielaltmaier")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bandreeva\\b", replacement = "mirraandreeva")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bgermani\\b", replacement = "germany")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bdaniel altmaier\\b", replacement = "danielaltmaier")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmirra andreeva\\b", replacement = "mirraandreeva")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\broland garro\\b", replacement = "rolandgarro")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bfabric santoro\\b", replacement = "fabricsantoro")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\barnaud clement\\b", replacement = "arnaudclement")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bserena william\\b", replacement = "serenawilliam")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bgrigor dimitrov\\b", replacement = "grigordimitrov")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmartina hingi\\b", replacement = "martinahingis")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bdian parri\\b", replacement = "dianeparry")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bfranc\\b", replacement = "france")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bcoco gauff\\b", replacement = "cocogauff")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bgauff\\b", replacement = "cocogauff")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\biga swiatek\\b", replacement = "igaswiatek")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bjulia grabher\\b", replacement = "juliagrabher")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bsesil karatantcheva\\b", replacement = "sesilkaratantcheva")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bswiatek\\b", replacement = "igaswiatek")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bamerican\\b", replacement = "unitedstates")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bclair liur\\b", replacement = "claireliu")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bsinner\\b", replacement = "janniksinner")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bfritz\\b", replacement = "taylorfritz")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\btaylor fritz\\b", replacement = "taylorfritz")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\broland garro\\b", replacement = "rolandgarros")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bcourt philipp chatrier\\b", 
                     replacement = "courtphilippchatrier")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\barthur rinderknech\\b", replacement = "arthurrinderknech")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\brinderknech\\b", replacement = "arthurrinderknech")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmarion bartoli\\b", replacement = "marionbartoli")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmartina hingi\\b", replacement = "martinahingis")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bruud\\b", replacement = "casperruud")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\brafa nadal\\b", replacement = "rafanadal")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bcasper ruud\\b", replacement = "casperruud")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\belena rybakina\\b", replacement = "elenarybakina")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\blinda noskova\\b", replacement = "lindanoskova")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bgiulio zeppieri\\b", replacement = "giuliozeppieri")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bchines\\b", replacement = "china")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bzhang zhizhen\\b", replacement = "zhangzhizhen")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bargentin\\b", replacement = "argentina")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bthiago agustin tirant\\b", 
                     replacement = "thiagoagustintirante")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bsuzann lenglen cup\\b", replacement = "suzannelenglencup")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bkayla day\\b", replacement = "kayladay")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmadison key\\b", replacement = "madisonkey")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bgerman\\b", replacement = "germany")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\balexand zverev\\b", replacement = "alexanderzverev")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bslovakian\\b", replacement = "slovakia")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\balex molcan\\b", replacement = "alexmolcan")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bfranc tiafo\\b", replacement = "francestiafoe")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\baslan karatsev\\b", replacement = "aslankaratsev")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bcroatian\\b", replacement = "croatia")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bborna coric\\b", replacement = "bornacoric")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bpedro cachin\\b", replacement = "pedrocachin")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\baustralian\\b", replacement = "autralia")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\balex de minaur\\b", replacement = "alexdeminaur")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\btoma martin etcheverri\\b", 
                     replacement = "tomasmartinetcheverry")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\btunisian\\b", replacement = "tunisia")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bon jabeur\\b", replacement = "onsjabeur")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bocean dodin\\b", replacement = "oceanedodin")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bwang xinyu\\b", replacement = "wangxinyu")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bpari\\b", replacement = "paris")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bhong yang\\b", replacement = "wenghongyang")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bbadminton asia championship\\b", replacement = "bac")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmalaysia open\\b", replacement = "open")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bgoh jin wei\\b", replacement = "gohjinwei")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bwang zhi yi\\b", replacement = "wangzhiyi")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bbueno air\\b", replacement = "buenoair")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bkyohei yamashitanaru shinoya\\b", replacement = "kyoheiyamashitanarushinoya")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\brobin tabelingselena piek\\b", replacement = "robintabelingselenapiek")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bchief execut offic ceo\\b", replacement = "ceo")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bbadminton associ malaysia\\b", replacement = "bam")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bdatuk jame selvaraj\\b", replacement = "datukjamesselvaraj")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmichell chai\\b", replacement = "michellechai")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bwong choong hann\\b", replacement = "wongchoonhhann")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\brexi mainaki\\b", replacement = "rexymainaki")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmisbun sidek\\b", replacement = "misbunsidek")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bjame\\b", replacement = "datukjamesselvaraj")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmichell\\b", replacement = "michellechai")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bdr tim jone\\b", replacement = "drtimjone")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bkirandeep kaur\\b", replacement = "kirandeepkaur")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bnur shamin azureen\\b", replacement = "nurshaminazureen")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bnur nabila alia yussaini\\b", replacement = "nurnabilaaliayussaini")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bsiti nur atika shaikh maznan\\b", replacement = "sitinuratikashaikhmaznan")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bnurul fatin fatiah azman\\b", replacement = "nurulfatinfatiahazman")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bthibatharshini jame\\b", replacement = "thibatharshinijames")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bdian nursyakira najwa\\b", replacement = "diannursyakiranajwa")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bkhairunnisa ayuni\\b", replacement = "khairunnisaayuni")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bdian nursyakira\\b", replacement = "diannursyakira")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bzati alyani zubir\\b", replacement = "zatialyanizubir")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bnorshafiqha ishak\\b", replacement = "norshafiqhaishak")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bdian nursyakira najwa\\b", replacement = "diannursyakiranajwa")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\baliana\\b", replacement = "alianaazizan")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\baliana azizan\\b", replacement = "alianaazizan")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bbukit jalil sport schoo\\b", replacement = "bukitjalilsportschool")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bizzah izzati asri\\b", replacement = "nurulizzahizzatimohdasri")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bani amira rosidi\\b", replacement = "aniamirarosidi")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmilton leg nation cup\\b", replacement = "miltonlegnationcup")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bdatuk seri abdul karim rahman hamzah\\b", replacement = "datukseriabdulkarimrahmanhamzah")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bdatuk abu samah abdul wahab\\b", replacement = "datukabusamahabdulwahab")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bmalaysia game\\b", replacement = "malaysiagame")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\babu samah\\b", replacement = "datukabusamahabdulwahab")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bbull\\b", replacement = "redbull")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bgeorg\\b", replacement = "georgerussell")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\brussel\\b", replacement = "georgerussell")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bnorri\\b", replacement = "cameronnorrie")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bgianpiero lambias\\b", replacement = "gianpierolambiase")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\bisl man tt race\\b", replacement = "islemanttrace")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\braul torra martinez\\b", replacement = "raultorrasmartinez")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\btorra martinez\\b", replacement = "raultorrasmartinez")
sport_news <- tm_map(sport_news, content_transformer(gsub), pattern = "\\braul\\b", replacement = "raultorrasmartinez")
writeLines(as.character(sport_news[[1]]))

### remove custom stopwords
byeStopwords <- c("can", "said", "saw", "go", "dont", "want", "howev", 
                  "now", "ask", "will", "just", "despit", "well", "also", 
                  "know", "say", "led", "comefrombehind", "sinc", "ad", 
                  "got", "im", "well", "th", "st", "nd", "rd", "make", 
                  "hes", "might", "even", "though","still", "see", "need", 
                  "cant", "like", "other", "rm", "minut", "last", "year",
                  "id", "wouldnt", "come", "put", "yet", "must", "ever",
                  "yearold")
sport_news <- tm_map(sport_news, removeWords, byeStopwords)
writeLines(as.character(sport_news[[1]]))

### remove extra whitespaces again because the text corpus consists of extra whitespaces
sport_news <- tm_map(sport_news, stripWhitespace)
writeLines(as.character(sport_news[[1]]))

### EXPORT CLEANED CORPUS DATA FOR THE USED OF TASK 2
### writeCorpus(sport_news, path = "C:/Users/User/Downloads/Task2")



## Document-term matrix
sport_dtm <- DocumentTermMatrix(sport_news) #Create document term matrix
sport_dtm

################################################################################

# Task 1: Perform topic modelling analysis using LDA
## Latent Dirichlet Allocation (LDA) Topic Modelling
## 1. Using the dataset created in Data acquisition section, create four topics, k = 4.
library(topicmodels)
sport_lda <- LDA(sport_dtm, k = 4, control = list(seed = 1234))
sport_lda



## Objective 1: To perform the per-topic-per-word probabilities of sports news articles
## 2. i. Extract per-topic per word probabilities and visualize at least eight (8) terms that are most common within each topic.
### extract per-topic per word probabilities
library(tidytext)
sport_topics <- tidy(sport_lda, matrix = "beta")
head(sport_topics, n = 8)



## Objective 2: To visualise the most common terms of sports news articles within each topic
library(tidyverse)
sport_top_terms <- sport_topics %>% group_by(topic) %>% top_n(8, beta) %>% 
  ungroup () %>% arrange (topic, -beta)
head(sport_top_terms, n = 32)

sport_top_terms %>% mutate(term = reorder(term,beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) + geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free") + coord_flip()



## Objective 3: To determine the greatest difference between the two topics
## 2. ii. Extract the relevant beta spread.
#### the greatest difference between topic 1 and 3.
beta_spread <- sport_topics %>% mutate (topic = paste0("topic", topic)) %>% spread(topic, beta) %>%
  filter (topic1 > 0.0075 | topic3 > 0.0075) %>% mutate(log_ratio = log2(topic3/topic1))

beta_spread %>% mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) + geom_col(show.legend = F) + coord_flip()

### the greatest difference between topic 2 and 4.
beta_spread <- sport_topics %>% mutate (topic = paste0("topic", topic)) %>% spread(topic, beta) %>%
  filter (topic2 > 0.0075 | topic4 > 0.0075) %>% mutate(log_ratio = log2(topic4/topic2))

beta_spread %>% mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) + geom_col(show.legend = F) + coord_flip()



## Objective 4: To perform the per-document-per-topic probabilities of sports news articles
## 2. iii. Perform per-document per topic probabilities.
sport_documents <- tidy(sport_lda, matrix = "gamma")
sport_documents$docs <- rep(1:dim(sport_dtm)[1], 4)
head(sport_documents, n = 8)



## Objective 5: To determine the types of sports news articles within each topic
### create a data frame with gamma results
sport_documents_df <- data.frame(sport_documents)

### plot gamma results
ggplot(data = sport_documents_df, aes(x = docs, y= gamma, 
                                      group = factor(topic), color = factor(topic))) +
  geom_line() + 
  scale_x_continuous(breaks=seq(1,36,1)) + 
  labs(color = "Topic") +
  facet_wrap(~ factor(topic), ncol = 1)


### topic 1
merged_df <- merge(tidy(sport_dtm), sport_documents_df, by = "document")
topic1_doc1 <- merged_df %>% filter(docs == "1") %>% arrange(desc(count))
topic1_doc2 <- merged_df %>% filter(docs == "3") %>% arrange(desc(count))
topic1_doc3 <- merged_df %>% filter(docs == "9") %>% arrange(desc(count))
topic1_doc4 <- merged_df %>% filter(docs == "11") %>% arrange(desc(count))
topic1_doc5 <- merged_df %>% filter(docs == "18") %>% arrange(desc(count))
topic1_doc6 <- merged_df %>% filter(docs == "25") %>% arrange(desc(count))
topic1_doc7 <- merged_df %>% filter(docs == "36") %>% arrange(desc(count))

top_term_doc1 <- head(unique(topic1_doc1$term[order(topic1_doc1$count, decreasing = T)]), 8)
top_term_doc2 <- head(unique(topic1_doc2$term[order(topic1_doc2$count, decreasing = T)]), 8)
top_term_doc3 <- head(unique(topic1_doc3$term[order(topic1_doc3$count, decreasing = T)]), 8)
top_term_doc4 <- head(unique(topic1_doc4$term[order(topic1_doc4$count, decreasing = T)]), 8)
top_term_doc5 <- head(unique(topic1_doc5$term[order(topic1_doc5$count, decreasing = T)]), 8)
top_term_doc6 <- head(unique(topic1_doc6$term[order(topic1_doc6$count, decreasing = T)]), 8)
top_term_doc7 <- head(unique(topic1_doc7$term[order(topic1_doc7$count, decreasing = T)]), 8)

topic1_df <- data.frame(doc1 = top_term_doc1, 
                        doc2 = top_term_doc2,
                        doc3 = top_term_doc3,
                        doc4 = top_term_doc4,
                        doc5 = top_term_doc5,
                        doc6 = top_term_doc6,
                        doc7 = top_term_doc7)

# Change column names based on chapter numbers
doc <- c(1, 3, 9, 11, 18, 25, 36)
new_column_names <- paste0("doc", doc)
colnames(topic1_df) <- new_column_names
topic1_df


### topic 2
topic2_doc1 <- merged_df %>% filter(docs == "5") %>% arrange(desc(count))
topic2_doc2 <- merged_df %>% filter(docs == "19") %>% arrange(desc(count))
topic2_doc3 <- merged_df %>% filter(docs == "20") %>% arrange(desc(count))
topic2_doc4 <- merged_df %>% filter(docs == "21") %>% arrange(desc(count))
topic2_doc5 <- merged_df %>% filter(docs == "22") %>% arrange(desc(count))
topic2_doc6 <- merged_df %>% filter(docs == "23") %>% arrange(desc(count))
topic2_doc7 <- merged_df %>% filter(docs == "24") %>% arrange(desc(count))
topic2_doc8 <- merged_df %>% filter(docs == "26") %>% arrange(desc(count))
topic2_doc9 <- merged_df %>% filter(docs == "27") %>% arrange(desc(count))

top_term_doc1 <- head(unique(topic2_doc1$term[order(topic2_doc1$count, decreasing = T)]), 8)
top_term_doc2 <- head(unique(topic2_doc2$term[order(topic2_doc2$count, decreasing = T)]), 8)
top_term_doc3 <- head(unique(topic2_doc3$term[order(topic2_doc3$count, decreasing = T)]), 8)
top_term_doc4 <- head(unique(topic2_doc4$term[order(topic2_doc4$count, decreasing = T)]), 8)
top_term_doc5 <- head(unique(topic2_doc5$term[order(topic2_doc5$count, decreasing = T)]), 8)
top_term_doc6 <- head(unique(topic2_doc6$term[order(topic2_doc6$count, decreasing = T)]), 8)
top_term_doc7 <- head(unique(topic2_doc7$term[order(topic2_doc7$count, decreasing = T)]), 8)
top_term_doc8 <- head(unique(topic2_doc8$term[order(topic2_doc8$count, decreasing = T)]), 8)
top_term_doc9 <- head(unique(topic2_doc9$term[order(topic2_doc9$count, decreasing = T)]), 8)

topic2_df <- data.frame(doc1 = top_term_doc1, 
                        doc2 = top_term_doc2,
                        doc3 = top_term_doc3,
                        doc4 = top_term_doc4,
                        doc5 = top_term_doc5,
                        doc6 = top_term_doc6,
                        doc7 = top_term_doc7,
                        doc8 = top_term_doc8,
                        doc9 = top_term_doc9)

doc <- c(5, 19, 20, 21, 22, 23, 24, 26, 27)
new_column_names <- paste0("doc", doc)
colnames(topic2_df) <- new_column_names
topic2_df


### topic 3
topic3_doc1 <- merged_df %>% filter(docs == "4") %>% arrange(desc(count))
topic3_doc2 <- merged_df %>% filter(docs == "6") %>% arrange(desc(count))
topic3_doc3 <- merged_df %>% filter(docs == "8") %>% arrange(desc(count))
topic3_doc4 <- merged_df %>% filter(docs == "10") %>% arrange(desc(count))
topic3_doc5 <- merged_df %>% filter(docs == "13") %>% arrange(desc(count))
topic3_doc6 <- merged_df %>% filter(docs == "14") %>% arrange(desc(count))
topic3_doc7 <- merged_df %>% filter(docs == "15") %>% arrange(desc(count))
topic3_doc8 <- merged_df %>% filter(docs == "16") %>% arrange(desc(count))
topic3_doc9 <- merged_df %>% filter(docs == "17") %>% arrange(desc(count))

top_term_doc1 <- head(unique(topic3_doc1$term[order(topic3_doc1$count, decreasing = T)]), 8)
top_term_doc2 <- head(unique(topic3_doc2$term[order(topic3_doc2$count, decreasing = T)]), 8)
top_term_doc3 <- head(unique(topic3_doc3$term[order(topic3_doc3$count, decreasing = T)]), 8)
top_term_doc4 <- head(unique(topic3_doc4$term[order(topic3_doc4$count, decreasing = T)]), 8)
top_term_doc5 <- head(unique(topic3_doc5$term[order(topic3_doc5$count, decreasing = T)]), 8)
top_term_doc6 <- head(unique(topic3_doc6$term[order(topic3_doc6$count, decreasing = T)]), 8)
top_term_doc7 <- head(unique(topic3_doc7$term[order(topic3_doc7$count, decreasing = T)]), 8)
top_term_doc8 <- head(unique(topic3_doc8$term[order(topic3_doc8$count, decreasing = T)]), 8)
top_term_doc9 <- head(unique(topic3_doc9$term[order(topic3_doc9$count, decreasing = T)]), 8)

topic3_df <- data.frame(doc1 = top_term_doc1, 
                        doc2 = top_term_doc2,
                        doc3 = top_term_doc3,
                        doc4 = top_term_doc4,
                        doc5 = top_term_doc5,
                        doc6 = top_term_doc6,
                        doc7 = top_term_doc7,
                        doc8 = top_term_doc8,
                        doc9 = top_term_doc9)

doc <- c(4, 6, 8, 10, 13, 14, 15, 16, 17)
new_column_names <- paste0("doc", doc)
colnames(topic3_df) <- new_column_names
topic3_df


### topic 4
topic4_doc1 <- merged_df %>% filter(docs == "2") %>% arrange(desc(count))
topic4_doc2 <- merged_df %>% filter(docs == "7") %>% arrange(desc(count))
topic4_doc3 <- merged_df %>% filter(docs == "12") %>% arrange(desc(count))
topic4_doc4 <- merged_df %>% filter(docs == "29") %>% arrange(desc(count))
topic4_doc5 <- merged_df %>% filter(docs == "30") %>% arrange(desc(count))
topic4_doc6 <- merged_df %>% filter(docs == "32") %>% arrange(desc(count))
topic4_doc7 <- merged_df %>% filter(docs == "33") %>% arrange(desc(count))
topic4_doc8 <- merged_df %>% filter(docs == "34") %>% arrange(desc(count))
topic4_doc9 <- merged_df %>% filter(docs == "35") %>% arrange(desc(count))

top_term_doc1 <- head(unique(topic4_doc1$term[order(topic4_doc1$count, decreasing = T)]), 8)
top_term_doc2 <- head(unique(topic4_doc2$term[order(topic4_doc2$count, decreasing = T)]), 8)
top_term_doc3 <- head(unique(topic4_doc3$term[order(topic4_doc3$count, decreasing = T)]), 8)
top_term_doc4 <- head(unique(topic4_doc4$term[order(topic4_doc4$count, decreasing = T)]), 8)
top_term_doc5 <- head(unique(topic4_doc5$term[order(topic4_doc5$count, decreasing = T)]), 8)
top_term_doc6 <- head(unique(topic4_doc6$term[order(topic4_doc6$count, decreasing = T)]), 8)
top_term_doc7 <- head(unique(topic4_doc7$term[order(topic4_doc7$count, decreasing = T)]), 8)
top_term_doc8 <- head(unique(topic4_doc8$term[order(topic4_doc8$count, decreasing = T)]), 8)
top_term_doc9 <- head(unique(topic4_doc9$term[order(topic4_doc9$count, decreasing = T)]), 8)

topic4_df <- data.frame(doc1 = top_term_doc1, 
                        doc2 = top_term_doc2,
                        doc3 = top_term_doc3,
                        doc4 = top_term_doc4,
                        doc5 = top_term_doc5,
                        doc6 = top_term_doc6,
                        doc7 = top_term_doc7,
                        doc8 = top_term_doc8,
                        doc9 = top_term_doc9)

doc <- c(2, 7, 12, 29, 30, 32, 33, 34, 35)
new_column_names <- paste0("doc", doc)
colnames(topic4_df) <- new_column_names
topic4_df
