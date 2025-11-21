### Network Traffic Analysis Script ###
# Network Analysis --------------------------------------------------------

## Another Overview --------------------------------------------------------

# domain accessed during network activity
merged_data_all %>%
  group_by(domain) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=30)

merged_data_ct_off %>%
  group_by(domain) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=30)

merged_data_ct_on %>%
  group_by(domain) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=30)

# application responsible for network activity
merged_data_all %>%
  group_by(bundleID) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

merged_data_ct_off %>%
  group_by(bundleID) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

merged_data_ct_on %>%
  group_by(bundleID) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

# domain owner
merged_data_all %>%
  group_by(domainOwner) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=30)

merged_data_ct_off %>%
  group_by(domainOwner) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

merged_data_ct_on %>%
  group_by(domainOwner) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)


# 1. Adding columns with simpler names ---------------------------------------

# 1.1 bundleID - App Name -----------------------------------------------------

# Start with bundleID

## Complete Df -----------------------------------------------------

# Renaming bundleID for easier readability
merged_data_all %>%
  group_by(bundleID) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=184)

library(dplyr)

merged_data_all <- merged_data_all %>%
  mutate(AppName = case_when(
    bundleID == "com.9gag.ios.mobile" ~ "9GAG",
    bundleID == "com.burbn.instagram" ~ "Instagram",
    bundleID == "com.brave.ios.browser" ~ "Brave",
    bundleID == "com.apple.AppStore" ~ "App Store",
    bundleID == "ch.migros.m-go" ~ "Migros",
    bundleID == "ch.tutti.iphone" ~ "Tutti",
    bundleID == "co.bird.Ride" ~ "Bird",
    bundleID == "com.adobe.Adobe-Reader" ~ "Acrobat",
    bundleID == "com.apple.mobilemail" ~ "Apple Mail",
    bundleID == "com.atebits.Tweetie2" ~ "Twitter",
    #bundleID == "com.bbc.mobile.news" ~ "BBC News",
    bundleID == "org.mozilla.ios.Firefox" ~ "Firefox",
    bundleID == "com.datacamp" ~ "DataCamp",
    bundleID == "com.duolingo.DuolingoMobile" ~ "Duolingo",
    bundleID == "com.google.Gmail" ~ "Gmail",
    bundleID == "com.google.ios.youtube" ~ "Youtube",
    bundleID == "com.google.Maps" ~ "Google Maps",
    bundleID == "com.linkedin.LinkedIn" ~ "LinkedIn",
    bundleID == "com.shallotgames.coffeegolf" ~ "Coffee Golf",
    bundleID == "com.spotify.client" ~ "Spotify",
    bundleID == "com.strava.stravaride" ~ "Strava",
    bundleID == "com.toyopagroup.picaboo" ~ "Snapchat",
    bundleID == "com.tripodsocial.apps.tandem" ~ "Tandem",
    bundleID == "company.thebrowser.ArcMobile2" ~ "Arc Search",
    bundleID == "de.spiegel.spon" ~ "DER SPIEGEL",
    bundleID == "net.whatsapp.WhatsApp" ~ "WhatsApp",
    bundleID == "org.whispersystems.signal" ~ "Signal",
    bundleID == "swiss.ricardo.iphone" ~ "Ricardo",
    bundleID == "tv.sf.iapp" ~ "Play SRF",
    bundleID == "com.go-tellm.tellm" ~ "Jodel",
    bundleID == "com.alibaba.iAliexpress" ~ "AliExpress",
    bundleID == "com.yourcompany.20minutes" ~ "20 Minuten",
    bundleID == "ch.search.iapp" ~ "Search.ch",
    bundleID == "com.nfl.gamecenter" ~ "NFL",
    bundleID == "com.apple.mobilesafari" ~ "Safari",
    bundleID == "ch.mote.swisspost" ~ "Post",
    #bundleID == "ch.postfinance.mobile" ~ "PostFinance",
    bundleID == "com.amazon.Lassen" ~ "Amazon Kindle",
    bundleID == "com.zhiliaoapp.musically" ~ "TikTok",
    bundleID == "com.tencent.xin" ~ "WeChat",
    bundleID == "com.microsoft.azureauthenticator" ~ "Microsoft Authenticator",
    bundleID == "fr.lemonde.LeMonde" ~ "Le Monde",
    bundleID == "ai.perplexity.app" ~ "Perplexity",
    # new additions
    bundleID == "com.thetrainline.iphone" ~ "Trainline",
    bundleID == "com.swissquote.Yuh" ~ "Yuh",
    bundleID == "com.microsoft.Office.Outlook" ~ "Outlook",
    bundleID == "com.google.GoogleMobile" ~ "Google App",
    bundleID == "de.komoot.berlinbikeapp" ~ "Komoot",
    bundleID == "ch.blick.Blick" ~ "Blick",
    bundleID == "ch.rts.rtsinfo" ~ "RTS",
    bundleID == "com.reddit.Reddit" ~ "Reddit",
    bundleID == "com.neonbanking.app" ~ "Neon",
    bundleID == "com.apple.iBooks" ~ "Apple Books",
    bundleID == "com.bonnie.trafficescape" ~ "Traffic Escape",
    bundleID == "com.revolut.revolut" ~ "Revolut",
    bundleID == "org.iggymedia.periodtracker" ~ "Flo",
    bundleID == "net.nextbike.official2012" ~ "Nextbike",
    bundleID == "com.apple.reminders" ~ "Apple Reminders",
    bundleID == "5Q4J53EFRC.com.sbb.ch" ~ "SBB",
    bundleID == "com.einnovation.temu" ~ "Temu",
    bundleID == "com.hammerandchisel.discord" ~ "Discord",
    bundleID == "at.bergfex.touren" ~ "bergfex",
    bundleID == "com.amazon.echo" ~ "Amazon Echo",
    bundleID == "com.apple.Music" ~ "Apple Music",
    bundleID == "com.e2ndesign.TPremium2" ~ "AppBox Pro",
    bundleID == "com.apple.podcasts" ~ "Apple Podcasts",
    bundleID == "com.ebay.iphone" ~ "eBay",
    bundleID == "com.ridedott.rider" ~ "Dott",
    bundleID == "com.alibaba.sourcing" ~ "Alibaba",
    bundleID == "com.apple.mobilenotes" ~ "Apple Notes",
    bundleID == "ch.digitec.nativeApp" ~ "Digitec",
    bundleID == "com.apple.Passwords" ~ "Apple Passwords",
    bundleID == "com.mobilityware.SolitaireFree" ~ "Solitaire",
    bundleID == "com.ookla.speedtest" ~ "Speedtest",
    bundleID == "com.proximabeta.aoemobile" ~ "AoE Mobile",
    bundleID == "com.squareenixmontreal.hitmansniper" ~ "Hitman",
    bundleID == "uk.co.bbc.news" ~ "BBC",
    bundleID == "com.ubercab.UberClient" ~ "Uber",
    bundleID == "com.openai.chat" ~ "ChatGPT",
    bundleID == "com.apple.weather" ~ "Apple Weather",
    bundleID == "com.facebook.Facebook" ~ "Facebook",
    bundleID == "com.8bit.bitwarden" ~ "Bitwarden",
    bundleID == "com.apple.Fitness" ~ "Apple Fitness",
    bundleID == "com.eatch.mobileapp" ~ "Just Eat",
    bundleID == "com.facebook.Messenger" ~ "Messenger",
    bundleID == "com.apple.mobilecal" ~ "Apple Calendar",
    bundleID == "ch.galaxus.nativeApp" ~ "Galaxus",
    bundleID == "com.voiapp.voi" ~ "Voi",
    bundleID == "to.freedom.FreedomBlocker" ~ "Freedom",
    bundleID == "io.worldwidemobility.trafikpoint" ~ "Trafikpoint",
    bundleID == "com.burbn.barcelona" ~ "Threads",
    bundleID == "ch.admin.meteoswiss" ~ "MeteoSwiss",
    bundleID == "com.apple.stocks" ~ "Apple Stocks",
    bundleID == "com.ubs.Paymit" ~ "UBS Twint",
    bundleID == "com.audible.iphone" ~ "Audible",
    bundleID == "com.valvesoftware.Steam" ~ "Steam",
    bundleID == "com.apple.Passbook" ~ "Apple Wallet",
    bundleID == "com.amazon.AmazonDE" ~ "Amazon",
    bundleID == "com.audionowdigital.player.bbcworldservice" ~ "BBCWorldService",
    bundleID == "com.apple.mobilephone" ~ "Phone App",
    bundleID == "ch.sac.sac" ~ "SAC",
    bundleID == "com.limebike" ~ "Lime",
    bundleID == "com.microsoft.skydrive" ~ "OneDrive",
    bundleID == "ch.workingbicycle.app" ~ "Working Bicycle",
    bundleID == "com.apple.mobileslideshow" ~ "Apple Photos",
    bundleID == "ch.salt.my" ~ "Salt",
    bundleID == "org.mozilla.ios.Klar" ~ "Firefox Klar",
    bundleID == "com.tacx.tacxtraining" ~ "Tacx Training",
    bundleID == "net.faz.FAZ" ~ "FAZ",
    bundleID == "ch.srf.srfplayer" ~ "SRF Player",
    bundleID == "com.google.Drive" ~ "Google Drive",
    bundleID == "exodus-movement.exodus" ~ "Exodus",
    bundleID == "com.apple.Maps" ~ "Apple Maps",
    bundleID == "xyz.blueskyweb.app" ~ "Bluesky",
    bundleID == "ch.slf.whiteriskmobile" ~ "WhiteRisk",
    bundleID == "com.shazam.Shazam" ~ "Shazam",
    bundleID == "com.apple.shortcuts" ~ "Apple Shortcuts",
    bundleID == "com.ubercab.UberEats" ~ "Uber Eats",
    bundleID == "com.zwift.Zwift" ~ "Zwift",
    bundleID == "com.google.Authenticator" ~ "Google Authenticator",
    bundleID == "com.linguee.DeepLMobileTranslator" ~ "DeepL",
    bundleID == "jp.co.sony.songpal.mdr" ~ "Sony Headphones",
    bundleID == "com.roamresearch.relemma" ~ "RoamResearch",
    bundleID == "ch.relai.relai" ~ "Relai",
    bundleID == "ch.watson.app.news" ~ "Watson",
    bundleID == "com.apple.Health" ~ "Apple Health",
    bundleID == "com.apple.MobileSMS" ~ "Messages App",
    bundleID == "notion.id" ~ "Notion",
    bundleID == "org.leo.org.leo.dict.01" ~ "LEO",
    bundleID == "ch.vermoegenszentrum.fipo" ~ "VZ Finanzportal",
    bundleID == "com.apple.findmy" ~ "Apple Find My",
    bundleID == "com.consumedbycode.slopes" ~ "Slopes",
    bundleID == "com.youneedabudget.evergreen.YNAB-Evergreen" ~ "YNAB",
    bundleID == "ch.coop.supercard" ~ "Coop Supercard",
    bundleID == "com.grammarly.keyboard" ~ "Grammarly",
    bundleID == "com.jamtech.smood" ~ "Smood",
    bundleID == "com.moxco.bumble" ~ "Bumble",
    bundleID == "fm.brainwave" ~ "Brainwave",
    bundleID == "ch.admin.swisstopo.swisstopo" ~ "Swisstopo",
    bundleID == "ch.local.search.launcher" ~ "Local.ch",
    bundleID == "ch.sonect.app" ~ "Sonect",
    bundleID == "com.yourcompany.PPClient" ~ "PayPal",
    bundleID == "ch.flatfox.ios" ~ "Flatfox",
    bundleID == "com.cardify.tinder" ~ "Tinder",
    bundleID == "com.nintendo.zara" ~ "Super Mario Run",
    bundleID == "ph.telegra.Telegraph" ~ "Telegraph",
    bundleID == "com.apple.Translate" ~ "Apple Translate",
    bundleID == "com.transferwise.Transferwise" ~ "Wise",
    bundleID == "net.Foddy.GettingOverIt" ~ "Getting Over It",
    bundleID == "com.getdropbox.Dropbox" ~ "Dropbox",
    bundleID == "com.zwift.ZwiftGame" ~ "Zwift",
    bundleID == "us.zoom.videomeetings" ~ "Zoom",
    bundleID == "co.hinge.mobile.ios" ~ "Hinge",
    bundleID == "com.ndemiccreations.plagueinc" ~ "Plague.Inc",
    bundleID == "trainerday.turbo" ~ "TrainerDay",
    bundleID == "ch.bankcler.zak" ~ "Zak",
    bundleID == "ch.yuh.twint" ~ "Yuh Twint",
    bundleID == "com.apple.MobileAddressBook" ~ "Contacts App",
    bundleID == "com.avira.passwordmanager" ~ "Avira PWM",
    bundleID == "com.google.Docs" ~ "Google Docs",
    bundleID == "com.google.gemini" ~ "Gemini",
    bundleID == "ch.post.postcardcreator" ~ "Postcard Creator",
    bundleID == "io.bluewallet.bluewallet" ~ "BlueWallet",
    bundleID == "com.anthropic.claude" ~ "Claude",
    bundleID == "com.sixgroup.debixplus" ~ "DebiX",
    bundleID == "com.windytv.ios" ~ "Windy",
    bundleID == "org.wikimedia.wikipedia" ~ "Wikipedia",
    bundleID == "com.crowscrowscrows.stanley" ~ "Stanley Parable",
    bundleID == "com.rileytestut.Delta" ~ "Delta Emulator",
    bundleID == "ch.toppreise.tpiphone" ~ "TopPreise",
    bundleID == "com.apple.news" ~ "Apple News",
    bundleID == "com.deepseek.chat" ~ "DeepSeek",
    bundleID == "com.calm.calmapp" ~ "Calm",
    bundleID == "de.incentify.DailyBudget" ~ "DailyBudget",
    bundleID == "dk.gomore" ~ "GoMore",
    bundleID == "com.FlukeDude.TheImpossibleGame" ~ "The Impossible Game",
    bundleID == "com.apple.music.classical" ~ "Apple Music Classical",
    bundleID == "com.ustwo.monumentvalley" ~ "Monument Valley",
    bundleID == "com.wikihow.wikiHowApp" ~ "wikiHow",
    bundleID == "io.frogg.Boulder-Buddy" ~ "Boulder Buddy",
    bundleID == "com.andreasilliger.tinywings" ~ "Tiny Wings",
    bundleID == "com.wakingup.wakingup" ~ "Waking Up",
    bundleID == "com.airbnb.app" ~ "Airbnb",
    bundleID == "com.mlobodzinski.Stoic" ~ "Stoic",
    bundleID == "com.playdeadgames.inside.ios" ~ "INSIDE",
    bundleID == "ch.peakfinder.peakfinder-alps" ~ "PeakFinder",
    bundleID == "com.apple.Bridge" ~ "Apple Watch",
    bundleID == "com.apple.compass" ~ "Apple Compass",
    bundleID == "com.apple.freeform" ~ "Apple Freeform",
    bundleID == "com.blinkslabs.Blinkist" ~ "Blinkist",
    bundleID == "com.grailr.CARROTweather" ~ "CARROT Weather",
    #
    TRUE ~ bundleID
  )
  )

merged_data_all <- merged_data_all %>% # moving AppName column next to bundleID column
  select(firstTimeStamp, timeStamp, hits, bundleID, AppName, domain, domainOwner, everything())    
  

## Same for CT off ------------------------------------------------

merged_data_ct_off <- merged_data_ct_off %>%
  mutate(AppName = case_when(
    bundleID == "com.9gag.ios.mobile" ~ "9GAG",
    bundleID == "com.burbn.instagram" ~ "Instagram",
    bundleID == "com.brave.ios.browser" ~ "Brave",
    bundleID == "com.apple.AppStore" ~ "App Store",
    bundleID == "ch.migros.m-go" ~ "Migros",
    bundleID == "ch.tutti.iphone" ~ "Tutti",
    bundleID == "co.bird.Ride" ~ "Bird",
    bundleID == "com.adobe.Adobe-Reader" ~ "Acrobat",
    bundleID == "com.apple.mobilemail" ~ "Apple Mail",
    bundleID == "com.atebits.Tweetie2" ~ "Twitter",
    #bundleID == "com.bbc.mobile.news" ~ "BBC News",
    bundleID == "org.mozilla.ios.Firefox" ~ "Firefox",
    bundleID == "com.datacamp" ~ "DataCamp",
    bundleID == "com.duolingo.DuolingoMobile" ~ "Duolingo",
    bundleID == "com.google.Gmail" ~ "Gmail",
    bundleID == "com.google.ios.youtube" ~ "Youtube",
    bundleID == "com.google.Maps" ~ "Google Maps",
    bundleID == "com.linkedin.LinkedIn" ~ "LinkedIn",
    bundleID == "com.shallotgames.coffeegolf" ~ "Coffee Golf",
    bundleID == "com.spotify.client" ~ "Spotify",
    bundleID == "com.strava.stravaride" ~ "Strava",
    bundleID == "com.toyopagroup.picaboo" ~ "Snapchat",
    bundleID == "com.tripodsocial.apps.tandem" ~ "Tandem",
    bundleID == "company.thebrowser.ArcMobile2" ~ "Arc Search",
    bundleID == "de.spiegel.spon" ~ "DER SPIEGEL",
    bundleID == "net.whatsapp.WhatsApp" ~ "WhatsApp",
    bundleID == "org.whispersystems.signal" ~ "Signal",
    bundleID == "swiss.ricardo.iphone" ~ "Ricardo",
    bundleID == "tv.sf.iapp" ~ "Play SRF",
    bundleID == "com.go-tellm.tellm" ~ "Jodel",
    bundleID == "com.alibaba.iAliexpress" ~ "AliExpress",
    bundleID == "com.yourcompany.20minutes" ~ "20 Minuten",
    bundleID == "ch.search.iapp" ~ "Search.ch",
    bundleID == "com.nfl.gamecenter" ~ "NFL",
    bundleID == "com.apple.mobilesafari" ~ "Safari",
    bundleID == "ch.mote.swisspost" ~ "Post",
    #bundleID == "ch.postfinance.mobile" ~ "PostFinance",
    bundleID == "com.amazon.Lassen" ~ "Amazon Kindle",
    bundleID == "com.zhiliaoapp.musically" ~ "TikTok",
    bundleID == "com.tencent.xin" ~ "WeChat",
    bundleID == "com.microsoft.azureauthenticator" ~ "Microsoft Authenticator",
    bundleID == "fr.lemonde.LeMonde" ~ "Le Monde",
    bundleID == "ai.perplexity.app" ~ "Perplexity",
    # new additions
    bundleID == "com.thetrainline.iphone" ~ "Trainline",
    bundleID == "com.swissquote.Yuh" ~ "Yuh",
    bundleID == "com.microsoft.Office.Outlook" ~ "Outlook",
    bundleID == "com.google.GoogleMobile" ~ "Google App",
    bundleID == "de.komoot.berlinbikeapp" ~ "Komoot",
    bundleID == "ch.blick.Blick" ~ "Blick",
    bundleID == "ch.rts.rtsinfo" ~ "RTS",
    bundleID == "com.reddit.Reddit" ~ "Reddit",
    bundleID == "com.neonbanking.app" ~ "Neon",
    bundleID == "com.apple.iBooks" ~ "Apple Books",
    bundleID == "com.bonnie.trafficescape" ~ "Traffic Escape",
    bundleID == "com.revolut.revolut" ~ "Revolut",
    bundleID == "org.iggymedia.periodtracker" ~ "Flo",
    bundleID == "net.nextbike.official2012" ~ "Nextbike",
    bundleID == "com.apple.reminders" ~ "Apple Reminders",
    bundleID == "5Q4J53EFRC.com.sbb.ch" ~ "SBB",
    bundleID == "com.einnovation.temu" ~ "Temu",
    bundleID == "com.hammerandchisel.discord" ~ "Discord",
    bundleID == "at.bergfex.touren" ~ "bergfex",
    bundleID == "com.amazon.echo" ~ "Amazon Echo",
    bundleID == "com.apple.Music" ~ "Apple Music",
    bundleID == "com.e2ndesign.TPremium2" ~ "AppBox Pro",
    bundleID == "com.apple.podcasts" ~ "Apple Podcasts",
    bundleID == "com.ebay.iphone" ~ "eBay",
    bundleID == "com.ridedott.rider" ~ "Dott",
    bundleID == "com.alibaba.sourcing" ~ "Alibaba",
    bundleID == "com.apple.mobilenotes" ~ "Apple Notes",
    bundleID == "ch.digitec.nativeApp" ~ "Digitec",
    bundleID == "com.apple.Passwords" ~ "Apple Passwords",
    bundleID == "com.mobilityware.SolitaireFree" ~ "Solitaire",
    bundleID == "com.ookla.speedtest" ~ "Speedtest",
    bundleID == "com.proximabeta.aoemobile" ~ "AoE Mobile",
    bundleID == "com.squareenixmontreal.hitmansniper" ~ "Hitman",
    bundleID == "uk.co.bbc.news" ~ "BBC",
    bundleID == "com.ubercab.UberClient" ~ "Uber",
    bundleID == "com.openai.chat" ~ "ChatGPT",
    bundleID == "com.apple.weather" ~ "Apple Weather",
    bundleID == "com.facebook.Facebook" ~ "Facebook",
    bundleID == "com.8bit.bitwarden" ~ "Bitwarden",
    bundleID == "com.apple.Fitness" ~ "Apple Fitness",
    bundleID == "com.eatch.mobileapp" ~ "Just Eat",
    bundleID == "com.facebook.Messenger" ~ "Messenger",
    bundleID == "com.apple.mobilecal" ~ "Apple Calendar",
    bundleID == "ch.galaxus.nativeApp" ~ "Galaxus",
    bundleID == "com.voiapp.voi" ~ "Voi",
    bundleID == "to.freedom.FreedomBlocker" ~ "Freedom",
    bundleID == "io.worldwidemobility.trafikpoint" ~ "Trafikpoint",
    bundleID == "com.burbn.barcelona" ~ "Threads",
    bundleID == "ch.admin.meteoswiss" ~ "MeteoSwiss",
    bundleID == "com.apple.stocks" ~ "Apple Stocks",
    bundleID == "com.ubs.Paymit" ~ "UBS Twint",
    bundleID == "com.audible.iphone" ~ "Audible",
    bundleID == "com.valvesoftware.Steam" ~ "Steam",
    bundleID == "com.apple.Passbook" ~ "Apple Wallet",
    bundleID == "com.amazon.AmazonDE" ~ "Amazon",
    bundleID == "com.audionowdigital.player.bbcworldservice" ~ "BBCWorldService",
    bundleID == "com.apple.mobilephone" ~ "Phone App",
    bundleID == "ch.sac.sac" ~ "SAC",
    bundleID == "com.limebike" ~ "Lime",
    bundleID == "com.microsoft.skydrive" ~ "OneDrive",
    bundleID == "ch.workingbicycle.app" ~ "Working Bicycle",
    bundleID == "com.apple.mobileslideshow" ~ "Apple Photos",
    bundleID == "ch.salt.my" ~ "Salt",
    bundleID == "org.mozilla.ios.Klar" ~ "Firefox Klar",
    bundleID == "com.tacx.tacxtraining" ~ "Tacx Training",
    bundleID == "net.faz.FAZ" ~ "FAZ",
    bundleID == "ch.srf.srfplayer" ~ "SRF Player",
    bundleID == "com.google.Drive" ~ "Google Drive",
    bundleID == "exodus-movement.exodus" ~ "Exodus",
    bundleID == "com.apple.Maps" ~ "Apple Maps",
    bundleID == "xyz.blueskyweb.app" ~ "Bluesky",
    bundleID == "ch.slf.whiteriskmobile" ~ "WhiteRisk",
    bundleID == "com.shazam.Shazam" ~ "Shazam",
    bundleID == "com.apple.shortcuts" ~ "Apple Shortcuts",
    bundleID == "com.ubercab.UberEats" ~ "Uber Eats",
    bundleID == "com.zwift.Zwift" ~ "Zwift",
    bundleID == "com.google.Authenticator" ~ "Google Authenticator",
    bundleID == "com.linguee.DeepLMobileTranslator" ~ "DeepL",
    bundleID == "jp.co.sony.songpal.mdr" ~ "Sony Headphones",
    bundleID == "com.roamresearch.relemma" ~ "RoamResearch",
    bundleID == "ch.relai.relai" ~ "Relai",
    bundleID == "ch.watson.app.news" ~ "Watson",
    bundleID == "com.apple.Health" ~ "Apple Health",
    bundleID == "com.apple.MobileSMS" ~ "Messages App",
    bundleID == "notion.id" ~ "Notion",
    bundleID == "org.leo.org.leo.dict.01" ~ "LEO",
    bundleID == "ch.vermoegenszentrum.fipo" ~ "VZ Finanzportal",
    bundleID == "com.apple.findmy" ~ "Apple Find My",
    bundleID == "com.consumedbycode.slopes" ~ "Slopes",
    bundleID == "com.youneedabudget.evergreen.YNAB-Evergreen" ~ "YNAB",
    bundleID == "ch.coop.supercard" ~ "Coop Supercard",
    bundleID == "com.grammarly.keyboard" ~ "Grammarly",
    bundleID == "com.jamtech.smood" ~ "Smood",
    bundleID == "com.moxco.bumble" ~ "Bumble",
    bundleID == "fm.brainwave" ~ "Brainwave",
    bundleID == "ch.admin.swisstopo.swisstopo" ~ "Swisstopo",
    bundleID == "ch.local.search.launcher" ~ "Local.ch",
    bundleID == "ch.sonect.app" ~ "Sonect",
    bundleID == "com.yourcompany.PPClient" ~ "PayPal",
    bundleID == "ch.flatfox.ios" ~ "Flatfox",
    bundleID == "com.cardify.tinder" ~ "Tinder",
    bundleID == "com.nintendo.zara" ~ "Super Mario Run",
    bundleID == "ph.telegra.Telegraph" ~ "Telegraph",
    bundleID == "com.apple.Translate" ~ "Apple Translate",
    bundleID == "com.transferwise.Transferwise" ~ "Wise",
    bundleID == "net.Foddy.GettingOverIt" ~ "Getting Over It",
    bundleID == "com.getdropbox.Dropbox" ~ "Dropbox",
    bundleID == "com.zwift.ZwiftGame" ~ "Zwift",
    bundleID == "us.zoom.videomeetings" ~ "Zoom",
    bundleID == "co.hinge.mobile.ios" ~ "Hinge",
    bundleID == "com.ndemiccreations.plagueinc" ~ "Plague.Inc",
    bundleID == "trainerday.turbo" ~ "TrainerDay",
    bundleID == "ch.bankcler.zak" ~ "Zak",
    bundleID == "ch.yuh.twint" ~ "Yuh Twint",
    bundleID == "com.apple.MobileAddressBook" ~ "Contacts App",
    bundleID == "com.avira.passwordmanager" ~ "Avira PWM",
    bundleID == "com.google.Docs" ~ "Google Docs",
    bundleID == "com.google.gemini" ~ "Gemini",
    bundleID == "ch.post.postcardcreator" ~ "Postcard Creator",
    bundleID == "io.bluewallet.bluewallet" ~ "BlueWallet",
    bundleID == "com.anthropic.claude" ~ "Claude",
    bundleID == "com.sixgroup.debixplus" ~ "DebiX",
    bundleID == "com.windytv.ios" ~ "Windy",
    bundleID == "org.wikimedia.wikipedia" ~ "Wikipedia",
    bundleID == "com.crowscrowscrows.stanley" ~ "Stanley Parable",
    bundleID == "com.rileytestut.Delta" ~ "Delta Emulator",
    bundleID == "ch.toppreise.tpiphone" ~ "TopPreise",
    bundleID == "com.apple.news" ~ "Apple News",
    bundleID == "com.deepseek.chat" ~ "DeepSeek",
    bundleID == "com.calm.calmapp" ~ "Calm",
    bundleID == "de.incentify.DailyBudget" ~ "DailyBudget",
    bundleID == "dk.gomore" ~ "GoMore",
    bundleID == "com.FlukeDude.TheImpossibleGame" ~ "The Impossible Game",
    bundleID == "com.apple.music.classical" ~ "Apple Music Classical",
    bundleID == "com.ustwo.monumentvalley" ~ "Monument Valley",
    bundleID == "com.wikihow.wikiHowApp" ~ "wikiHow",
    bundleID == "io.frogg.Boulder-Buddy" ~ "Boulder Buddy",
    bundleID == "com.andreasilliger.tinywings" ~ "Tiny Wings",
    bundleID == "com.wakingup.wakingup" ~ "Waking Up",
    bundleID == "com.airbnb.app" ~ "Airbnb",
    bundleID == "com.mlobodzinski.Stoic" ~ "Stoic",
    bundleID == "com.playdeadgames.inside.ios" ~ "INSIDE",
    bundleID == "ch.peakfinder.peakfinder-alps" ~ "PeakFinder",
    bundleID == "com.apple.Bridge" ~ "Apple Watch",
    bundleID == "com.apple.compass" ~ "Apple Compass",
    bundleID == "com.apple.freeform" ~ "Apple Freeform",
    bundleID == "com.blinkslabs.Blinkist" ~ "Blinkist",
    bundleID == "com.grailr.CARROTweather" ~ "CARROT Weather",
    #
    TRUE ~ bundleID #))
  ) 
  )  

merged_data_ct_off <- merged_data_ct_off %>%
  select(firstTimeStamp, timeStamp, hits, bundleID, AppName, domain, domainOwner, everything())    
  
## Same for CT on ------------------------------------------------

merged_data_ct_on <- merged_data_ct_on %>%
  mutate(AppName = case_when(
    bundleID == "com.9gag.ios.mobile" ~ "9GAG",
    bundleID == "com.burbn.instagram" ~ "Instagram",
    bundleID == "com.brave.ios.browser" ~ "Brave",
    bundleID == "com.apple.AppStore" ~ "App Store",
    bundleID == "ch.migros.m-go" ~ "Migros",
    bundleID == "ch.tutti.iphone" ~ "Tutti",
    bundleID == "co.bird.Ride" ~ "Bird",
    bundleID == "com.adobe.Adobe-Reader" ~ "Acrobat",
    bundleID == "com.apple.mobilemail" ~ "Apple Mail",
    bundleID == "com.atebits.Tweetie2" ~ "Twitter",
    #bundleID == "com.bbc.mobile.news" ~ "BBC News",
    bundleID == "org.mozilla.ios.Firefox" ~ "Firefox",
    bundleID == "com.datacamp" ~ "DataCamp",
    bundleID == "com.duolingo.DuolingoMobile" ~ "Duolingo",
    bundleID == "com.google.Gmail" ~ "Gmail",
    bundleID == "com.google.ios.youtube" ~ "Youtube",
    bundleID == "com.google.Maps" ~ "Google Maps",
    bundleID == "com.linkedin.LinkedIn" ~ "LinkedIn",
    bundleID == "com.shallotgames.coffeegolf" ~ "Coffee Golf",
    bundleID == "com.spotify.client" ~ "Spotify",
    bundleID == "com.strava.stravaride" ~ "Strava",
    bundleID == "com.toyopagroup.picaboo" ~ "Snapchat",
    bundleID == "com.tripodsocial.apps.tandem" ~ "Tandem",
    bundleID == "company.thebrowser.ArcMobile2" ~ "Arc Search",
    bundleID == "de.spiegel.spon" ~ "DER SPIEGEL",
    bundleID == "net.whatsapp.WhatsApp" ~ "WhatsApp",
    bundleID == "org.whispersystems.signal" ~ "Signal",
    bundleID == "swiss.ricardo.iphone" ~ "Ricardo",
    bundleID == "tv.sf.iapp" ~ "Play SRF",
    bundleID == "com.go-tellm.tellm" ~ "Jodel",
    bundleID == "com.alibaba.iAliexpress" ~ "AliExpress",
    bundleID == "com.yourcompany.20minutes" ~ "20 Minuten",
    bundleID == "ch.search.iapp" ~ "Search.ch",
    bundleID == "com.nfl.gamecenter" ~ "NFL",
    bundleID == "com.apple.mobilesafari" ~ "Safari",
    bundleID == "ch.mote.swisspost" ~ "Post",
    #bundleID == "ch.postfinance.mobile" ~ "PostFinance",
    bundleID == "com.amazon.Lassen" ~ "Amazon Kindle",
    bundleID == "com.zhiliaoapp.musically" ~ "TikTok",
    bundleID == "com.tencent.xin" ~ "WeChat",
    bundleID == "com.microsoft.azureauthenticator" ~ "Microsoft Authenticator",
    bundleID == "fr.lemonde.LeMonde" ~ "Le Monde",
    bundleID == "ai.perplexity.app" ~ "Perplexity",
    # new additions
    bundleID == "com.thetrainline.iphone" ~ "Trainline",
    bundleID == "com.swissquote.Yuh" ~ "Yuh",
    bundleID == "com.microsoft.Office.Outlook" ~ "Outlook",
    bundleID == "com.google.GoogleMobile" ~ "Google App",
    bundleID == "de.komoot.berlinbikeapp" ~ "Komoot",
    bundleID == "ch.blick.Blick" ~ "Blick",
    bundleID == "ch.rts.rtsinfo" ~ "RTS",
    bundleID == "com.reddit.Reddit" ~ "Reddit",
    bundleID == "com.neonbanking.app" ~ "Neon",
    bundleID == "com.apple.iBooks" ~ "Apple Books",
    bundleID == "com.bonnie.trafficescape" ~ "Traffic Escape",
    bundleID == "com.revolut.revolut" ~ "Revolut",
    bundleID == "org.iggymedia.periodtracker" ~ "Flo",
    bundleID == "net.nextbike.official2012" ~ "Nextbike",
    bundleID == "com.apple.reminders" ~ "Apple Reminders",
    bundleID == "5Q4J53EFRC.com.sbb.ch" ~ "SBB",
    bundleID == "com.einnovation.temu" ~ "Temu",
    bundleID == "com.hammerandchisel.discord" ~ "Discord",
    bundleID == "at.bergfex.touren" ~ "bergfex",
    bundleID == "com.amazon.echo" ~ "Amazon Echo",
    bundleID == "com.apple.Music" ~ "Apple Music",
    bundleID == "com.e2ndesign.TPremium2" ~ "AppBox Pro",
    bundleID == "com.apple.podcasts" ~ "Apple Podcasts",
    bundleID == "com.ebay.iphone" ~ "eBay",
    bundleID == "com.ridedott.rider" ~ "Dott",
    bundleID == "com.alibaba.sourcing" ~ "Alibaba",
    bundleID == "com.apple.mobilenotes" ~ "Apple Notes",
    bundleID == "ch.digitec.nativeApp" ~ "Digitec",
    bundleID == "com.apple.Passwords" ~ "Apple Passwords",
    bundleID == "com.mobilityware.SolitaireFree" ~ "Solitaire",
    bundleID == "com.ookla.speedtest" ~ "Speedtest",
    bundleID == "com.proximabeta.aoemobile" ~ "AoE Mobile",
    bundleID == "com.squareenixmontreal.hitmansniper" ~ "Hitman",
    bundleID == "uk.co.bbc.news" ~ "BBC",
    bundleID == "com.ubercab.UberClient" ~ "Uber",
    bundleID == "com.openai.chat" ~ "ChatGPT",
    bundleID == "com.apple.weather" ~ "Apple Weather",
    bundleID == "com.facebook.Facebook" ~ "Facebook",
    bundleID == "com.8bit.bitwarden" ~ "Bitwarden",
    bundleID == "com.apple.Fitness" ~ "Apple Fitness",
    bundleID == "com.eatch.mobileapp" ~ "Just Eat",
    bundleID == "com.facebook.Messenger" ~ "Messenger",
    bundleID == "com.apple.mobilecal" ~ "Apple Calendar",
    bundleID == "ch.galaxus.nativeApp" ~ "Galaxus",
    bundleID == "com.voiapp.voi" ~ "Voi",
    bundleID == "to.freedom.FreedomBlocker" ~ "Freedom",
    bundleID == "io.worldwidemobility.trafikpoint" ~ "Trafikpoint",
    bundleID == "com.burbn.barcelona" ~ "Threads",
    bundleID == "ch.admin.meteoswiss" ~ "MeteoSwiss",
    bundleID == "com.apple.stocks" ~ "Apple Stocks",
    bundleID == "com.ubs.Paymit" ~ "UBS Twint",
    bundleID == "com.audible.iphone" ~ "Audible",
    bundleID == "com.valvesoftware.Steam" ~ "Steam",
    bundleID == "com.apple.Passbook" ~ "Apple Wallet",
    bundleID == "com.amazon.AmazonDE" ~ "Amazon",
    bundleID == "com.audionowdigital.player.bbcworldservice" ~ "BBCWorldService",
    bundleID == "com.apple.mobilephone" ~ "Phone App",
    bundleID == "ch.sac.sac" ~ "SAC",
    bundleID == "com.limebike" ~ "Lime",
    bundleID == "com.microsoft.skydrive" ~ "OneDrive",
    bundleID == "ch.workingbicycle.app" ~ "Working Bicycle",
    bundleID == "com.apple.mobileslideshow" ~ "Apple Photos",
    bundleID == "ch.salt.my" ~ "Salt",
    bundleID == "org.mozilla.ios.Klar" ~ "Firefox Klar",
    bundleID == "com.tacx.tacxtraining" ~ "Tacx Training",
    bundleID == "net.faz.FAZ" ~ "FAZ",
    bundleID == "ch.srf.srfplayer" ~ "SRF Player",
    bundleID == "com.google.Drive" ~ "Google Drive",
    bundleID == "exodus-movement.exodus" ~ "Exodus",
    bundleID == "com.apple.Maps" ~ "Apple Maps",
    bundleID == "xyz.blueskyweb.app" ~ "Bluesky",
    bundleID == "ch.slf.whiteriskmobile" ~ "WhiteRisk",
    bundleID == "com.shazam.Shazam" ~ "Shazam",
    bundleID == "com.apple.shortcuts" ~ "Apple Shortcuts",
    bundleID == "com.ubercab.UberEats" ~ "Uber Eats",
    bundleID == "com.zwift.Zwift" ~ "Zwift",
    bundleID == "com.google.Authenticator" ~ "Google Authenticator",
    bundleID == "com.linguee.DeepLMobileTranslator" ~ "DeepL",
    bundleID == "jp.co.sony.songpal.mdr" ~ "Sony Headphones",
    bundleID == "com.roamresearch.relemma" ~ "RoamResearch",
    bundleID == "ch.relai.relai" ~ "Relai",
    bundleID == "ch.watson.app.news" ~ "Watson",
    bundleID == "com.apple.Health" ~ "Apple Health",
    bundleID == "com.apple.MobileSMS" ~ "Messages App",
    bundleID == "notion.id" ~ "Notion",
    bundleID == "org.leo.org.leo.dict.01" ~ "LEO",
    bundleID == "ch.vermoegenszentrum.fipo" ~ "VZ Finanzportal",
    bundleID == "com.apple.findmy" ~ "Apple Find My",
    bundleID == "com.consumedbycode.slopes" ~ "Slopes",
    bundleID == "com.youneedabudget.evergreen.YNAB-Evergreen" ~ "YNAB",
    bundleID == "ch.coop.supercard" ~ "Coop Supercard",
    bundleID == "com.grammarly.keyboard" ~ "Grammarly",
    bundleID == "com.jamtech.smood" ~ "Smood",
    bundleID == "com.moxco.bumble" ~ "Bumble",
    bundleID == "fm.brainwave" ~ "Brainwave",
    bundleID == "ch.admin.swisstopo.swisstopo" ~ "Swisstopo",
    bundleID == "ch.local.search.launcher" ~ "Local.ch",
    bundleID == "ch.sonect.app" ~ "Sonect",
    bundleID == "com.yourcompany.PPClient" ~ "PayPal",
    bundleID == "ch.flatfox.ios" ~ "Flatfox",
    bundleID == "com.cardify.tinder" ~ "Tinder",
    bundleID == "com.nintendo.zara" ~ "Super Mario Run",
    bundleID == "ph.telegra.Telegraph" ~ "Telegraph",
    bundleID == "com.apple.Translate" ~ "Apple Translate",
    bundleID == "com.transferwise.Transferwise" ~ "Wise",
    bundleID == "net.Foddy.GettingOverIt" ~ "Getting Over It",
    bundleID == "com.getdropbox.Dropbox" ~ "Dropbox",
    bundleID == "com.zwift.ZwiftGame" ~ "Zwift",
    bundleID == "us.zoom.videomeetings" ~ "Zoom",
    bundleID == "co.hinge.mobile.ios" ~ "Hinge",
    bundleID == "com.ndemiccreations.plagueinc" ~ "Plague.Inc",
    bundleID == "trainerday.turbo" ~ "TrainerDay",
    bundleID == "ch.bankcler.zak" ~ "Zak",
    bundleID == "ch.yuh.twint" ~ "Yuh Twint",
    bundleID == "com.apple.MobileAddressBook" ~ "Contacts App",
    bundleID == "com.avira.passwordmanager" ~ "Avira PWM",
    bundleID == "com.google.Docs" ~ "Google Docs",
    bundleID == "com.google.gemini" ~ "Gemini",
    bundleID == "ch.post.postcardcreator" ~ "Postcard Creator",
    bundleID == "io.bluewallet.bluewallet" ~ "BlueWallet",
    bundleID == "com.anthropic.claude" ~ "Claude",
    bundleID == "com.sixgroup.debixplus" ~ "DebiX",
    bundleID == "com.windytv.ios" ~ "Windy",
    bundleID == "org.wikimedia.wikipedia" ~ "Wikipedia",
    bundleID == "com.crowscrowscrows.stanley" ~ "Stanley Parable",
    bundleID == "com.rileytestut.Delta" ~ "Delta Emulator",
    bundleID == "ch.toppreise.tpiphone" ~ "TopPreise",
    bundleID == "com.apple.news" ~ "Apple News",
    bundleID == "com.deepseek.chat" ~ "DeepSeek",
    bundleID == "com.calm.calmapp" ~ "Calm",
    bundleID == "de.incentify.DailyBudget" ~ "DailyBudget",
    bundleID == "dk.gomore" ~ "GoMore",
    bundleID == "com.FlukeDude.TheImpossibleGame" ~ "The Impossible Game",
    bundleID == "com.apple.music.classical" ~ "Apple Music Classical",
    bundleID == "com.ustwo.monumentvalley" ~ "Monument Valley",
    bundleID == "com.wikihow.wikiHowApp" ~ "wikiHow",
    bundleID == "io.frogg.Boulder-Buddy" ~ "Boulder Buddy",
    bundleID == "com.andreasilliger.tinywings" ~ "Tiny Wings",
    bundleID == "com.wakingup.wakingup" ~ "Waking Up",
    bundleID == "com.airbnb.app" ~ "Airbnb",
    bundleID == "com.mlobodzinski.Stoic" ~ "Stoic",
    bundleID == "com.playdeadgames.inside.ios" ~ "INSIDE",
    bundleID == "ch.peakfinder.peakfinder-alps" ~ "PeakFinder",
    bundleID == "com.apple.Bridge" ~ "Apple Watch",
    bundleID == "com.apple.compass" ~ "Apple Compass",
    bundleID == "com.apple.freeform" ~ "Apple Freeform",
    bundleID == "com.blinkslabs.Blinkist" ~ "Blinkist",
    bundleID == "com.grailr.CARROTweather" ~ "CARROT Weather",
    #
    TRUE ~ bundleID
  )
  )

merged_data_ct_on <- merged_data_ct_on %>%
  select(firstTimeStamp, timeStamp, hits, bundleID, AppName, domain, domainOwner, everything())


# 1.2 DomainOwner -------------------------------------------------------------

# domain owner
merged_data_all %>%
  group_by(domainOwner) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=76)

# number of unique domainOwner
length(unique(merged_data_all$domainOwner))

# create new data frame with new column "DomainOwnerName" that renames all the domains that contain strings like "google", "apple", "facebook", "amazon", to that string
library(tidyverse) # For data manipulation


## Creating new column DomainOwnerName ---------------------------------------------------------
merged_data_all <- merged_data_all %>%
  mutate(DomainOwnerName = case_when(
    str_detect(domainOwner, "Microsoft Corporation") ~ "Microsoft",
    str_detect(domainOwner, "Adobe Inc.", ) ~ "Adobe",
    str_detect(domainOwner, "AddApptr GmbH") ~ "AddApptr",
    str_detect(domainOwner, "Google LLC") ~ "Google",
    str_detect(domainOwner, "Facebook, Inc.") ~ "Meta",
    str_detect(domainOwner, "Unity Software Inc.") ~ "Unity Inc.",
    # additional DomainOwners
    str_detect(domainOwner, "Braze, Inc.") ~ "Braze Inc.",
    str_detect(domainOwner, "Urban Airship, Inc.") ~ "United Airship Inc.",
    str_detect(domainOwner, "SAP SE") ~ "SAP SE",
    str_detect(domainOwner, "Kochava") ~ "Kochava",
    str_detect(domainOwner, "AppLovin Corporation") ~ "AppLovin Corp.",
    str_detect(domainOwner, "RevenueCat") ~ "RevenueCat",
    str_detect(domainOwner, "Vungle Inc") ~ "Vungle Inc.",
    str_detect(domainOwner, "Amplitude") ~ "Amplitude",
    str_detect(domainOwner, "Digital Turbine") ~ "Digital Turbine",
    str_detect(domainOwner, "Snap Inc.") ~ "Snap Inc.",
    str_detect(domainOwner, "Amazon Technologies, Inc.") ~ "Amazon",
    str_detect(domainOwner, "AppsFlyer") ~ "AppsFlyer",
    str_detect(domainOwner, "Twitter, Inc.") ~ "Twitter Inc.",
    str_detect(domainOwner, "ByteDance Ltd.") ~ "ByteDance Ltd.",
    str_detect(domainOwner, "CleverTap") ~ "CleverTap",
    str_detect(domainOwner, "Moloco Inc.") ~ "Moloco Inc.",
    str_detect(domainOwner, "Criteo SA") ~ "Criteo SA",
    str_detect(domainOwner, "DataDome") ~ "DataDome",
    str_detect(domainOwner, "Forter Inc.") ~ "Forter Inc.",
    str_detect(domainOwner, "Bugsnag Inc.") ~ "Bugsnag Inc.",
    str_detect(domainOwner, "Datadog, Inc.") ~ "Datadog Inc.",
    str_detect(domainOwner, "InMobi Pte Ltd") ~ "InMobi Pte",
    str_detect(domainOwner, "Index Exchange, Inc.") ~ "Index Exchange",
    str_detect(domainOwner, "PayPal, Inc.") ~ "PayPal",
    str_detect(domainOwner, "Reddit Inc.") ~ "Reddit",
    str_detect(domainOwner, "OneSignal") ~ "OneSignal",
    str_detect(domainOwner, "OpenX Technologies Inc") ~ "OpenX Inc.",
    str_detect(domainOwner, "YieldMo, Inc.") ~ "YieldMo Inc.",
    str_detect(domainOwner, "Alibaba Group") ~ "Alibaba Group",
    str_detect(domainOwner, "IPONWEB GmbH") ~ "IPONWEB",
    str_detect(domainOwner, "Magnite, Inc.") ~ "Magnite Inc.",
    str_detect(domainOwner, "Mixpanel, Inc.") ~ "Mixpanel Inc.",
    str_detect(domainOwner, "The Trade Desk Inc") ~ "Trade Desk Inc.",
    str_detect(domainOwner, "TripleLift") ~ "TripleLift",
    str_detect(domainOwner, "Prospect One") ~ "Prospect One",
    str_detect(domainOwner, "Segment.io, Inc.") ~ "Segment Inc.",
    str_detect(domainOwner, "Vizbee, Inc.") ~ "Vizbee Inc.",
    str_detect(domainOwner, "comScore, Inc") ~ "comScore Inc.",
    str_detect(domainOwner, "Cloudflare, Inc.") ~ "Cloudflare",
    str_detect(domainOwner, "Iterable, Inc.") ~ "Iterable",
    str_detect(domainOwner, "LiveRamp") ~ "LiveRamp",
    str_detect(domainOwner, "Mobvista") ~ "Mobvista",
    str_detect(domainOwner, "Pulsepoint, Inc.") ~ "Pulsepoint",
    str_detect(domainOwner, "Salesforce.com, Inc.") ~ "Salesforce",
    str_detect(domainOwner, "ShareThis, Inc") ~ "ShareThis Inc.",
    str_detect(domainOwner, "Sift Science, Inc.") ~ "Sift Science Inc.",
    str_detect(domainOwner, "Verizon Media") ~ "Verizon Media",
    str_detect(domainOwner, "Zeta Global") ~ "Zeta Global",
    str_detect(domainOwner, "Fastly, Inc.") ~ "Fastly",
    str_detect(domainOwner, "GumGum") ~ "GumGum",
    str_detect(domainOwner, "Helpshift, Inc.") ~ "Helpshift",
    str_detect(domainOwner, "ID5 Technology Ltd") ~ "ID5 Technology",
    str_detect(domainOwner, "LiveIntent Inc.") ~ "LiveIntent Inc.",
    str_detect(domainOwner, "Lotame Solutions, Inc.") ~ "Lotame Solutions",
    str_detect(domainOwner, "Propeller Ads") ~ "Propeller Ads",
    str_detect(domainOwner, "PubMatic, Inc.") ~ "PubMatic Inc.",
    str_detect(domainOwner, "Quantcast Corporation") ~ "Quantcast Corp.",
    str_detect(domainOwner, "Sharethrough, Inc.") ~ "Sharethrough Inc.",
    str_detect(domainOwner, "Singular Labs, Inc.") ~ "Singular Labs",
    str_detect(domainOwner, "Sovrn Holdings") ~ "Sovrn Holdings",
    str_detect(domainOwner, "Teads \\( Luxenbourg \\) SA") ~ "Teads",
    str_detect(domainOwner, "The Rubicon Project, Inc.") ~ "Rubicon Project",
    str_detect(domainOwner, "Throtle") ~ "Throtle",
    str_detect(domainOwner, "Trustpilot A/S") ~ "Trustpilot",
    str_detect(domainOwner, "Undertone Networks") ~ "Undertone Networks",
    str_detect(domainOwner, "WarnerMedia, LLC") ~ "WarnerMedia",
    str_detect(domainOwner, "Wingify") ~ "Wingify",
    str_detect(domainOwner, "Yieldlove GmbH") ~ "Yieldlove",

    # adding via domains
    str_detect(domain, "google") ~ "Google",
    str_detect(domain, "youtube") ~ "Google", 
    str_detect(domain, "gstatic") ~ "Google",
    str_detect(domain, "googleapis") ~ "Google",
    str_detect(domain, "app-analytics-services.com") ~ "Google",
    str_detect(domain, "firebase-settings.crashlytics.com") ~ "Google",
    str_detect(domain, "firebaselogging-pa.googleapis.com") ~ "Google",
    str_detect(domain, "g.doubleclick") ~ "Google",
    str_detect(domain, "ad.doubleclick.net") ~ "Google",
    str_detect(domain, "apple") ~ "Apple",
    str_detect(domain, "mzstatic") ~ "Apple",
    str_detect(domain, "icloud") ~ "Apple",
    str_detect(domain, "itunes") ~ "Apple",
    str_detect(domain, "facebook") ~ "Meta",
    str_detect(domain, "whatsapp") ~ "Meta",
    str_detect(domain, "instagram") ~ "Meta",
    str_detect(domain, "threads") ~ "Meta",
    str_detect(domain, "fbcdn") ~ "Meta",
    str_detect(domain, "giphy") ~ "Shutterstock",
    #str_detect(domain, "messenger") ~ "Meta",
    str_detect(domain, "amazon") ~ "Amazon",
    str_detect(domain, "microsoft") ~ "Microsoft",
    str_detect(domain, "linkedin") ~ "Microsoft",
    str_detect(domain, "bing") ~ "Microsoft",
    str_detect(domain, "skype") ~ "Microsoft",
    str_detect(domain, "adobe") ~ "Adobe",
    str_detect(domain, "appsflyer") ~ "AppsFlyer",
    str_detect(domain, "unity3d") ~ "Unity Software",
    str_detect(domain, "unity.com") ~ "Unity Software",
    str_detect(domain, "ingest.sentry.io") ~ "Functional Software",
    str_detect(domain, "twitter") ~ "Twitter Inc.",
    str_detect(domain, "tiktok") ~ "ByteDance Ltd.",
    str_detect(domain, "mozilla") ~ "Mozilla Firefox",
    str_detect(domain, "spotify") ~ "Spotify",
    str_detect(domain, "reddit") ~ "Reddit",
    str_detect(domain, "snapchat") ~ "Snap Inc.",
    str_detect(domain, "tutti") ~ "SMG AG",
    str_detect(domain, "ricardo") ~ "SMG AG",
    str_detect(domain, "jodel") ~ "Jodel Venture GmbH",
    str_detect(domain, "strava") ~ "Strava Inc.",
    str_detect(domain, "signal.org") ~ "Signal Technology Foundation",
    str_detect(domain, "brave.com") ~ "Brave Software Inc.",
    str_detect(domain, "spiegel") ~ "Rudolf Augstein GmbH & Co. KG",
    str_detect(domain, "aliexpress") ~ "Alibaba Group",
    str_detect(domain, "alibaba") ~ "Alibaba Group",
    str_detect(domain, "alipay") ~ "Alibaba Group",
    str_detect(domain, "alicdn") ~ "Alibaba Group",
    str_detect(domain, "aliapp") ~ "Alibaba Group",
    str_detect(domain, "qq.com") ~ "Tencent Holdings Limited",
    str_detect(domain, "wechat.com") ~ "Tencent Holdings Limited",
    str_detect(domain, "tencent") ~ "Tencent Holdings Limited",
    str_detect(domain, "twint") ~ "TWINT AG",
    str_detect(domain, "20min") ~ "TX Group AG",
    str_detect(domain, "migro") ~ "Migros-Genossenschafts-Bund (MGB)",
    str_detect(domain, "srgssr") ~ "SRG SRF",
    str_detect(domain, "digitec") ~ "Digitec Galaxus AG",
    str_detect(domain, "galaxus") ~ "Digitec Galaxus AG",
    str_detect(domain, "sbb.ch") ~ "SBB CFF FFS",
    str_detect(domain, "adnxs") ~ "Microsoft",
    str_detect(domain, "doubleclick") ~ "Google",
    str_detect(domain, "bsky") ~ "Bluesky Social PBC",
    str_detect(domain, "doubleverify") ~ "DoubleVerify Holdings Inc.",
    str_detect(domain, "lencr") ~ "Let's Encrypt",
    str_detect(domain, "onesignal") ~ "OneSignal",
    str_detect(domain, "api3.branch.io") ~ "Branch",
    str_detect(domain, "app.usercentrics.eu") ~ "Usercentrics GmbH",
    str_detect(domain, "cacerts.digicert.com") ~ "DigiCert",
    str_detect(domain, "cdn.branch.io") ~ "Branch",
    str_detect(domain, "cdn.cookielaw.org") ~ "OneTrust",
    str_detect(domain, "census-app-x.scorecardresearch.com") ~ "comScore Inc.",
    str_detect(domain, "config.mapbox.com") ~ "Mapbox",
    str_detect(domain, "ocsp.digicert.com") ~ "DigiCert",
    str_detect(domain, "outlook.office365.com") ~ "Microsoft",
    str_detect(domain, "prod-mediate-events.applovin.com") ~ "AppLovin Corp.",
    str_detect(domain, "region1.app-analytics-services-att.com") ~ "AT&T Inc.",
    str_detect(domain, "static.zdassets.com") ~ "Zendesk",
    
    
    TRUE ~ "Other"
  ))

merged_data_all <- merged_data_all %>% 
  select(firstTimeStamp, timeStamp, hits, bundleID, AppName, domain, domainOwner, DomainOwnerName, everything()) 
# Save df as CSV
write.csv(merged_data_all, "Output/Tables/merged_data_all_df.csv", row.names = TRUE)

## Printing DomainOwnerName summaries ------------------------------------------------------
library(dplyr)
merged_data_all %>%
  group_by(DomainOwnerName) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=104) %>%
  # add new empty "notes" column
  mutate(notes = case_when(
    TRUE ~ "" # Default case for all other domains
  )) %>%
  write.csv("Output/Tables/DomainOwnerName_summary.csv", row.names = TRUE)


merged_data_all %>%
  group_by(domain) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=150) 

## Filtered Df domain > 10 -------------------------------------------------------------

# New df with domains that have more than 10 accesses
merged_data_all_domain_10plus <- merged_data_all %>%
  group_by(domain) %>%
  summarise(total_accesses = n()) %>%
  filter(total_accesses > 10) %>%
  arrange(desc(total_accesses)) %>%
  # add DomainOwnerName and domainType columns
  left_join(merged_data_all %>% 
              select(domain, domainOwner, DomainOwnerName, domainType, domainClassification, initiatedType) %>%
              distinct(), by = "domain") %>%
  # add new empty "notes" column
  mutate(notes = case_when(
    TRUE ~ "" # Default case for all other domains
  )) %>%
  print(n=150)

# Save df as CSV
write.csv(merged_data_all_domain_10plus, "Output/Tables/most_accessed_domains_10plus.csv", row.names = TRUE)


# New df with domains that have more than 10 accesses and all selected columns
merged_data_all_domain_10plus_full <- merged_data_all %>%
  filter(domain %in% merged_data_all_domain_10plus$domain) %>%
  select(firstTimeStamp, timeStamp, hits, bundleID, AppName, domain, domainOwner, DomainOwnerName, everything()) %>%
  arrange(desc(timeStamp)) 

# Save df as CSV
write.csv(merged_data_all_domain_10plus_full, "Output/Tables/most_accessed_domains_10plus_full.csv", row.names = TRUE)


## New df's with domain Owners ---------------------------------------

# New df for domain Owner Names with domains that have more than 10 accesses
merged_data_all_domain_10plus_domainOwners <- merged_data_all_domain_10plus %>%
  #group_by(domainOwner) %>%
  group_by(DomainOwnerName) %>%
  summarise(total_accesses = n()) %>%
  #filter(total_accesses > 10) %>%
  arrange(desc(total_accesses)) %>%
  mutate(notes = case_when(   # add new empty "notes" column
    TRUE ~ "" # Default case for all other domains
  )) %>%
  print(n=150)
  
write.csv(merged_data_all_domain_10plus_domainOwners, "Output/Tables/DomainOwnerName_summary_10plus.csv", row.names = TRUE)

# Count domainType
table(merged_data_all$domainType)
table(merged_data_all_domain_10plus$domainType)

13375/100
1718/133.75

# unique number of domains where domainType = 1
length(unique(merged_data_all$domain[merged_data_all$domainType == 1])) # 246

length(unique(merged_data_all$domain[merged_data_all$domainType == 2])) # 4218

# unique number of bundleIDs
length(unique(merged_data_all$bundleID)) # 184


# 2. BlackLists for Cross-Referencing ------------------------

# Prep for Cross-Referencing DFs with black and white lists
library(dplyr)

# remove leading www. from domain in merged_data_all
merged_data_all <- merged_data_all %>%
  mutate(domain = str_remove(domain, "^www\\."))

#merged_data_all_more_info <- merged_data_all


# 2.1 Web list parsing -----------------------------------------------------

# --- Load required packages ---
library(httr)
library(readr)
library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)

# Paths to local files (if any)
tracker_paths <- list(
  disconnect_me = '/Users/nicolaswaser/New-project-GitHub-first/R/MSA II/Input Data/disconnect.txt'
)

# --- URLs of well-known tracker lists ---
tracker_urls <- list(
  easyprivacy = "https://easylist.to/easylist/easyprivacy.txt",
  stevenblack = "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts",
  disconnect_me = "https://raw.githubusercontent.com/disconnectme/disconnect-tracking-protection/master/services.json",
  masked_domain = "https://raw.githubusercontent.com/GoogleChrome/ip-protection/refs/heads/main/Masked-Domain-List.md",
  prigent_ads = "https://v.firebog.net/hosts/Prigent-Ads.txt",
  fademind = "https://raw.githubusercontent.com/FadeMind/hosts.extras/master/add.2o7Net/hosts",
  frogeye = "https://hostfiles.frogeye.fr/firstparty-trackers-hosts.txt",
  notrack = "https://gitlab.com/quidsup/notrack-blocklists/raw/master/notrack-blocklist.txt", # flagged as having to many false positives
  
  # newly added lists with ads:
  yoyo = "https://pgl.yoyo.org/adservers/serverlist.php?hostformat=hosts&mimetype=plaintext&useip=0.0.0.0", # domain start with 0.0.0.0
  dan_pollock = "https://someonewhocares.org/hosts/zero/hosts",
  anudeepND = "https://raw.githubusercontent.com/anudeepND/blacklist/master/adservers.txt",
  easylist = "https://easylist.to/easylist/easylist.txt",
  
  # soon discontinued / outdated lists:
  developerdan = "https://www.github.developerdan.com/hosts/lists/ads-and-tracking-extended.txt", # 429286 domains
  frogeye_multiparty = "https://hostfiles.frogeye.fr/multiparty-trackers-hosts.txt", # 44956 domains
  w3c = "https://raw.githubusercontent.com/Kees1958/W3C_annual_most_used_survey_blocklist/6b8c2411f22dda68b0b41757aeda10e50717a802/TOP_EU_US_Ads_Trackers_HOST",
  stevenblack_df_XL = "https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/fakenews-gambling-porn-social/hosts"
)


## Additional URLs ---------------------------------------------------------

## from uBlock Origin:
# Ad_guard tracking protection?
# first party tracking on non advertising sites https://raw.githubusercontent.com/AdguardTeam/AdguardFilters/refs/heads/master/SpywareFilter/sections/tracking_servers_firstparty.txt
# third party tracking https://github.com/AdguardTeam/AdguardFilters/blob/master/SpywareFilter/sections/tracking_servers.txt
# mobile analytics spyware https://raw.githubusercontent.com/AdguardTeam/AdguardFilters/refs/heads/master/SpywareFilter/sections/mobile.txt
# track param ($removeparam used for tracking what to do?) https://raw.githubusercontent.com/AdguardTeam/AdguardFilters/refs/heads/master/TrackParamFilter/sections/general_url.txt

## From Firebog.net:
# soon discontinued / outdated lists:
# https://www.github.developerdan.com/hosts/lists/ads-and-tracking-extended.txt 429286 domains
# https://hostfiles.frogeye.fr/multiparty-trackers-hosts.txt 44956 domains
# https://raw.githubusercontent.com/Kees1958/W3C_annual_most_used_survey_blocklist/6b8c2411f22dda68b0b41757aeda10e50717a802/TOP_EU_US_Ads_Trackers_HOST

# sus
#https://raw.githubusercontent.com/PolishFiltersTeam/KADhosts/master/KADhosts.txt
#https://raw.githubusercontent.com/FadeMind/hosts.extras/master/add.Spam/hosts
#https://v.firebog.net/hosts/static/w3kbl.txt

# ads
#https://adaway.org/hosts.txt
#https://v.firebog.net/hosts/AdguardDNS.txt
#https://v.firebog.net/hosts/Admiral.txt
#https://raw.githubusercontent.com/anudeepND/blacklist/master/adservers.txt
#https://v.firebog.net/hosts/Easylist.txt
#https://raw.githubusercontent.com/FadeMind/hosts.extras/master/UncheckyAds/hosts
#https://raw.githubusercontent.com/bigdargon/hostsVN/master/hosts

# malware
#https://raw.githubusercontent.com/DandelionSprout/adfilt/master/Alternate%20versions%20Anti-Malware%20List/AntiMalwareHosts.txt
#https://v.firebog.net/hosts/Prigent-Crypto.txt
#https://raw.githubusercontent.com/FadeMind/hosts.extras/master/add.Risk/hosts
#https://phishing.army/download/phishing_army_blocklist_extended.txt
#https://gitlab.com/quidsup/notrack-blocklists/raw/master/notrack-malware.txt
#https://v.firebog.net/hosts/RPiList-Malware.txt
#https://raw.githubusercontent.com/Spam404/lists/master/main-blacklist.txt
#https://raw.githubusercontent.com/AssoEchap/stalkerware-indicators/master/generated/hosts
#https://urlhaus.abuse.ch/downloads/hostfile/
#https://lists.cyberhost.uk/malware.txt


## Syntax Cleaning Functions -----------------------------------------------

# --- Helper function: clean and normalize domains ---
clean_domains <- function(domains) {
  domains %>%
    stringr::str_remove_all("^\\|\\|") %>%  # remove leading ||
    stringr::str_remove_all("^\\|") %>%     # remove leading |
    stringr::str_remove_all("\\^$") %>%     # remove trailing ^
    stringr::str_remove_all("/.*$") %>%     # remove anything after /
    stringr::str_to_lower() %>%             # lowercase
    stringr::str_remove("^www\\.") %>%      # remove leading www.
    stringr::str_remove_all("^\\&") %>%     # remove &
    stringr::str_remove_all("^\\.") %>%     # remove .
    stringr::str_remove_all("^\\?") %>%     # remove ?
    stringr::str_remove_all("^\\_") %>%     # remove _
    stringr::str_remove_all("\\?.*$") %>%   # remove query strings
    stringr::str_remove_all("#+.*$") %>%    # remove selectors like ## or ###
    stringr::str_remove_all(",domain.*$") %>% # remove ,domain and anything after
    stringr::str_remove_all("\\.$")         # remove trailing dot
}


# --- Parser for EasyPrivacy ---
get_easyprivacy <- function(url) {
  txt <- content(GET(url), as = "text")
  raw <- read_lines(I(txt))
  df <- data.frame(domain = raw, stringsAsFactors = FALSE) %>%
    filter(!str_starts(domain, "!")) %>%  # remove comments
    filter(!str_detect(domain, "\\*\\*\\*")) %>%  # remove comments
    filter(!str_starts(domain, "\\@\\@\\|\\|")) %>%  # remove exceptions
    mutate(domain = str_remove_all(domain, "\\^\\$third-party")) %>%    # remove trailing ^$ third-party
    mutate(domain = clean_domains(domain)) %>%
    filter(domain != "")
  return(df)
}


# --- Parser for StevenBlack hosts file ---
get_stevenblack <- function(url) {
  txt <- content(GET(url), as = "text")
  raw <- read_lines(I(txt))
  df <- data.frame(line = raw, stringsAsFactors = FALSE) %>%
    filter(str_detect(line, "^0\\.0\\.0\\.0\\s+")) %>% # lines with "0.0.0.0 domain"
    mutate(domain = str_remove(line, "^0\\.0\\.0\\.0\\s+")) %>%
    select(domain) %>%
    #mutate(domain = clean_domains(domain))
  return(df)
}


# --- Generic parser for plain text lists (ads/tracker hosts) ---
get_plainlist <- function(url) {
  txt <- content(GET(url), as = "text")
  raw <- read_lines(I(txt))
  df <- data.frame(line = raw, stringsAsFactors = FALSE) %>%
    # Remove comments and blank lines
    filter(!str_starts(line, "#")) %>%
    filter(line != "") %>%
    # Remove "0.0.0.0 " or "127.0.0.1 " if present
    mutate(domain = str_remove(line, "^(0\\.0\\.0\\.0|127\\.0\\.0\\.1)\\s+")) %>%
    select(domain) %>%
    #mutate(domain = clean_domains(domain)) %>%
    filter(domain != "")
  return(df)
}


# 2.2 Upload Lists from Files as df's ---------------------------------------

library(dplyr)
easyprivacy_df <- get_easyprivacy(tracker_urls$easyprivacy)
stevenblack_df <- get_stevenblack(tracker_urls$stevenblack)
masked_domain_df <- get_plainlist(tracker_urls$masked_domain)
prigent_df <- get_plainlist(tracker_urls$prigent_ads)
fademind_df <- get_plainlist(tracker_urls$fademind)
frogeye_df <- get_plainlist(tracker_urls$frogeye)
notrack_df <- get_plainlist(tracker_urls$notrack) # flagged as having to many false positives

# newly added lists with ads:
yoyo_df <- get_plainlist(tracker_urls$yoyo)
dan_pollock_df <- get_plainlist(tracker_urls$dan_pollock)
anudeepND_df <- get_plainlist(tracker_urls$anudeepND) 
easylist_df <- get_easyprivacy(tracker_urls$easylist)

# soon discontinued / outdated lists:
developerdan_df <- get_plainlist(tracker_urls$developerdan)
frogeye_multi_df <- get_plainlist(tracker_urls$frogeye_multiparty)
w3c_df <- get_plainlist(tracker_urls$w3c)
stevenblack_df_XL <- get_stevenblack(tracker_urls$stevenblack_df_XL)


## Disconnect from File ----------------------------------------------------

# Loading necessary libraries
library(jsonlite)
library(tidyverse)

# Parsing the JSON data
# Assuming the file is in your working directory
json_data <- fromJSON("Input Data/disconnect.txt", simplifyVector = FALSE)

# Creating an empty dataframe to store results
disconnect_df <- tibble()

# Iterating through each category
for (category_name in names(json_data$categories)) {
  category_array <- json_data$categories[[category_name]]
  
  # Iterating through each organization in the category
  for (org_obj in category_array) {
    # Getting the organization name (first key in the object)
    org_name <- names(org_obj)[1]
    
    # Getting the URL data for this organization
    url_obj <- org_obj[[org_name]]
    
    # Getting the URL (first key in the URL object)
    url <- names(url_obj)[1]
    
    # Getting the domains (array of domains for this URL)
    disconnect_domains <- url_obj[[url]]
    
    # Creating a temporary dataframe for this organization's domains
    disconnect_temp_df <- tibble(
      category = category_name,
      organisation = org_name,
      url = url,
      domain = disconnect_domains
    )
    
    # Adding to the result dataframe
    disconnect_df <- bind_rows(disconnect_df, disconnect_temp_df)
  }
}

rm(disconnect_temp_df, category_array, category_name, org_name, org_obj, url, 
   url_obj, disconnect_domains, json_data)

# Viewing the first few rows to check the result
head(disconnect_df)


## LockdownPrivacy JSON File Uploads --------------------------------------

# Parsing the JSON data
# Assuming the file is in your working directory
library(jsonlite)
library(dplyr)
library(stringr)

# load the JSON files
privacy_list <- fromJSON("Input Data/Lockdown Privacy Blocklists/privacyBlockList.json")

ad_list1 <- fromJSON("Input Data/Lockdown Privacy Blocklists/adBlockList.json")
ad_list2 <- fromJSON("Input Data/Lockdown Privacy Blocklists/adBlockListTwo.json")
ad_list3 <- fromJSON("Input Data/Lockdown Privacy Blocklists/adBlockListThree.json")

social_list <- fromJSON("Input Data/Lockdown Privacy Blocklists/socialBlockList.json")

# function to extract and clean domains from a single list
extract_domains <- function(JSONdata) {
  
  # filter for rules where action type is "block" and load-type is "third-party"
  filtered_rules <- JSONdata %>%
    filter(action$type == "block") %>%
    filter(sapply(trigger$`load-type`, function(x) "third-party" %in% x))
  
  # extract url-filter values
  url_filters <- filtered_rules$trigger$`url-filter`
  
  # remove regex patterns to get clean domains
  clean_domains <- url_filters %>%
    # remove ^https?://
    str_remove("^https?://") %>%
    # remove ^https?:/+
    str_remove("^https?:/+") %>%
    # remove ^[^:]+:(//)?
    str_remove("^[^:]+:(//)?") %>%
    # remove ([^/]+\\.)?
    str_remove("\\(\\[\\^/\\]\\+\\\\\\.\\)\\?") %>%
    # remove ([^-_.%a-z0-9].*)?$
    str_remove("\\(\\[\\^-_\\.%a-z0-9\\]\\.\\*\\)\\?\\$") %>%
    # remove ^[^:]+:(//)?.*&
    str_remove("^[^:]+:(//)?.*&") %>%
    # remove [:\/]
    str_remove("\\[:/\\\\]") %>%
    # remove any remaining backslashes
    str_remove_all("\\\\") %>%
    # remove [^/]+\\.
    str_remove_all("\\[\\^/\\]\\+\\.") %>%
    
    # remove *&np=
    str_remove_all("\\*&np=") %>%
    # remove *&hp=
    str_remove_all("\\*&hp=") %>%
    # remove *.se.*
    str_remove_all("\\*\\.se\\.\\*") %>%
    
    # remove any remaining brackets and parentheses
    str_remove_all("\\[|\\]|\\(|\\)") %>%
    # remove ^ and $ anchors
    str_remove_all("\\^|\\$") %>%
    # remove +
    str_remove_all("\\+") %>%
    # remove ?
    str_remove_all("\\?") %>%
    # remove :
    str_remove_all(":") %>%
    # remove =
    str_remove_all("=") %>%
    
    # remove //
    str_remove_all("//") %>%
    # remove /
    str_remove_all("/") %>%
    
    # remove . at beginning
    str_remove_all("^\\.") %>%
    # remove *
    str_remove_all("\\*") %>%
    # remove &
    str_remove_all("&") %>%
    
    # remove -_.%A-Za-z0-9.
    #str_remove("-_\\.\\%A-Za-z0-9\\.") %>%
    
    # trim whitespace
    str_trim()
  
  return(clean_domains)
}


# 2.3 Modifying the dfs -----------------------------------------------------

## NoTrack
notrack_df <- notrack_df %>%
  mutate(domain = str_squish(domain)) %>%                       # trim stray spaces
  separate(
    domain,
    into = c("domain", "DomainOwnerNoTrack"),
    sep = "\\s*#\\s*",                                          # split on '#' with optional spaces
    fill = "right",
    extra = "merge"                                             # keep everything after '#' together
  ) %>%
  mutate(DomainOwnerNoTrack = str_squish(DomainOwnerNoTrack))

notrack_df <- notrack_df %>%
  mutate(DomainOwnerNoTrack = str_squish(DomainOwnerNoTrack)) %>%                       
  separate(
    DomainOwnerNoTrack,
    into = c("DomainOwnerNoTrack", "CategoryNoTrack"),
    sep = "\\s*-\\s*",                                          
    fill = "right",
    extra = "merge"                                             
  ) %>%
  mutate(CategoryNoTrack = str_squish(CategoryNoTrack))


## Masked_domain 
masked_domain_df <- masked_domain_df %>%
  slice(-1:-36) # remove first text lines (header info)

masked_domain_df <- masked_domain_df %>%
  mutate(domain = str_squish(domain)) %>%                       # trim stray spaces
  separate(
    domain,
    into = c("domain", "DomainOwnerMaskedD"),
    sep = "\\s*\\|\\s*",                                        # split on '|' with optional spaces
    fill = "right",
    extra = "merge"                                             # keep everything after '|' together
  ) %>%
  mutate(DomainOwnerMaskedD = str_squish(DomainOwnerMaskedD))

masked_domain_df <- masked_domain_df %>%
  mutate(DomainOwnerMaskedD = str_squish(DomainOwnerMaskedD)) %>%                       
  separate(
    DomainOwnerMaskedD,
    into = c("DomainOwnerMaskedD", "NotesMaskedD"),
    sep = "\\s*\\|\\s*",                                          
    fill = "right",
    extra = "merge"                                             
  ) %>%
  mutate(NotesMaskedD = str_squish(NotesMaskedD))

# Structures of the DFs
str(easyprivacy_df)
str(stevenblack_df)
str(disconnect_df)
str(masked_domain_df)
str(prigent_df)
str(fademind_df)
str(frogeye_df)
str(notrack_df)
str(yoyo_df)
str(dan_pollock_df)
str(anudeepND_df)
str(easylist_df)

str(developerdan_df)
str(frogeye_multi_df)
str(w3c_df)


## Disconnect Data Clean --------------------------------------------

# convert disconnect tibble into df
disconnect_df <- as.data.frame(disconnect_df)
str(disconnect_df)

# convert domain values to character
disconnect_df$domain <- as.character(disconnect_df$domain)
str(disconnect_df)
# rename Category column to CategoryDisconnect
colnames(disconnect_df)[colnames(disconnect_df) == "category"] <- "CategoryDisconnect"
colnames(disconnect_df)[colnames(disconnect_df) == "organisation"] <- "OrganisationDisconnect"
colnames(disconnect_df)[colnames(disconnect_df) == "url"] <- "URLDisconnect"

# Show duplicate values in disconnect_df (domain column)
disconnect_duplicates <- disconnect_df %>%
  group_by(domain) %>%
  filter(n() > 1)
rm(disconnect_duplicates)

# if there are duplicate domains, melt them into one row with all CategoryDisconnect values in the same cell
collapsed_df <- disconnect_df %>%
  group_by(domain) %>% 
  summarise(
    CategoryDisconnect = paste(unique(CategoryDisconnect), collapse = ", "),
    OrganisationDisconnect = paste(unique(OrganisationDisconnect), collapse = ", "),
    URLDisconnect = paste(unique(URLDisconnect), collapse = ", "),
    .groups = "drop"
  )

disconnect_df_expanded <- disconnect_df
disconnect_df <- collapsed_df
rm(collapsed_df)


## LockdownPrivacy Extraction ---------------------------------------------

# extract domains from each list
privacy_domains <- extract_domains(privacy_list)
str(privacy_domains)
privacy_domains <- data.frame(
  domain = privacy_domains,
  category = "privacy", # Analytics, Web Statistics Counters, User tracking pixels, third party monitoring
  stringsAsFactors = FALSE)


ad1_domains <- extract_domains(ad_list1)
str(ad1_domains)
ad1_domains <- data.frame(
  domain = ad1_domains,
  category = "advertising", # advertising networks, ad servers, and marketing platforms (automation)
  stringsAsFactors = FALSE)

ad2_domains <- extract_domains(ad_list2)
str(ad2_domains)
ad2_domains <- data.frame(
  domain = ad2_domains,
  category = "advertising", # advertising networks, ad servers, and marketing platforms (automation)
  stringsAsFactors = FALSE)

ad3_domains <- extract_domains(ad_list3)
str(ad3_domains)
ad3_domains <- data.frame(
  domain = ad3_domains,
  category = "advertising", # advertising networks, ad servers, and marketing platforms (automation)
  stringsAsFactors = FALSE)


social_domains <- extract_domains(social_list)
str(social_domains)
social_domains <- data.frame(
  domain = social_domains,
  category = "social", # Blocks social media widgets embedded on third-party websites (Like and sharing buttons, comments, etc.)
  stringsAsFactors = FALSE)

rm(privacy_list, ad_list1, ad_list2, ad_list3, social_list)

# combine all domains into one df
lockdown_privacy_df <- bind_rows(
  privacy_domains, ad1_domains, ad2_domains, ad3_domains, social_domains) %>%
  distinct(domain, .keep_all = TRUE) %>%
  filter(domain != "" & !is.na(domain)) %>%
  # rename category column to CategoryLockdown
  rename(CategoryLockdown = category)


# 2.4 Combining all dfs into one master blacklist -----------------------------

rm(blacklist_df)
blacklist_df <- bind_rows( 
  easyprivacy_df, # trackers
  stevenblack_df, # adware, malware
  disconnect_df, # ads, trackers
  masked_domain_df, # trackers
  prigent_df, # ads, trackers
  fademind_df, # ads, trackers
  frogeye_df, # first party trackers
  
  # newly added lists:
  lockdown_privacy_df, # trackers, ads, and badware
  yoyo_df, # ads, trackers, badware
  dan_pollock_df, # ads, trackers, badware
  anudeepND_df, # ads, trackers, badware
) %>%
  distinct(domain, .keep_all = TRUE) %>%
  filter(domain != "" & !is.na(domain)) 

blacklist_df <- blacklist_df %>%
  slice(-1:-1)  # remove first line (header info)

# unique number of domains in blacklist_df
length(unique(blacklist_df$domain)) # 224'848

# --- Preview results ---
head(blacklist_df, 20)


## Adding extended Master Blacklist ----------------------------------------
rm(blacklist_XL_df)
blacklist_XL_df <- bind_rows(
  blacklist_df,
  developerdan_df, # outdated ads, trackers
  frogeye_multi_df, # outdated tracker
  w3c_df, # outdated ads, trackers
  stevenblack_df_XL, # adware, malware, fakenews, gambling
  easylist_df, # ads
  notrack_df, # ads, trackers - disencouraged due to too many false positives
  
) %>%
  distinct(domain, .keep_all = TRUE) %>%
  filter(domain != "" & !is.na(domain))

# unique number of domains in blacklist_XL_df
length(unique(blacklist_XL_df$domain)) #


# 2.5 Adding tracker info to merged_data_all_more_info -----------------------

library(dplyr)
merged_data_all_more_info <- merged_data_all
#rm(merged_data_all_more_info)


## Disconnect ----------------------------------------------------

merged_data_all_more_info <- merged_data_all_more_info %>%
  mutate(DisconnectTracker = domain %in% disconnect_df$domain)

merged_data_all_more_info <- merged_data_all_more_info %>%
  left_join(
    disconnect_df %>% 
      select(domain, OrganisationDisconnect, CategoryDisconnect), by = "domain") #%>%
  #distinct()

#count number of TRUE in disconnect_tracker
table(merged_data_all_more_info$DisconnectTracker)
table(merged_data_all_more_info$domainType)
table(merged_data_all_more_info$domainType, merged_data_all_more_info$DisconnectTracker)


## EasyPrivacy -----------------------------

library(dplyr)
# find all network activity entries that match EasyList EasyPrivacy and add a tracker flag 
merged_data_all_more_info <- merged_data_all_more_info %>%
  mutate(EasyPrivacyTracker = domain %in% easyprivacy_df$domain)

#count number of TRUE in easylist_tracker
table(merged_data_all_more_info$EasyPrivacyTracker)
table(merged_data_all_more_info$domainType)
table(merged_data_all_more_info$domainType, merged_data_all_more_info$EasyPrivacyTracker)


## Summary
#tracker_summary_easylist <- merged_data_all_more_info %>%
  #filter(EasyPrivacyTracker) %>%
  #group_by(domain) %>%
  #summarise(total_hits = n(), .groups = "drop") %>%
  #arrange(desc(total_hits))

#print(tracker_summary_easylist, n = 50)


## Stevenblack ---------------------------------------------------

merged_data_all_more_info <- merged_data_all_more_info %>%
  mutate(stevenblackTracker = domain %in% stevenblack_df$domain)

#count number of TRUE in easylist_tracker
table(merged_data_all_more_info$stevenblackTracker)
table(merged_data_all_more_info$domainType)
table(merged_data_all_more_info$domainType, merged_data_all_more_info$stevenblackTracker)


## Masked Domain -------------------------------------------------

merged_data_all_more_info <- merged_data_all_more_info %>%
  mutate(MaskedDomainTracker = domain %in% masked_domain_df$domain) %>%
  # add DomainOwnerMaskedD info column
  left_join(masked_domain_df %>% 
              select(domain, DomainOwnerMaskedD), by = "domain") #%>%
              #distinct()

#count number of TRUE in masked_domain_tracker
table(merged_data_all_more_info$MaskedDomainTracker)
table(merged_data_all_more_info$domainType)
table(merged_data_all_more_info$domainType, merged_data_all_more_info$MaskedDomainTracker)


## NoTrack -------------------------------------------------------

merged_data_all_more_info <- merged_data_all_more_info %>%
  mutate(NoTrackTracker = domain %in% notrack_df$domain)

merged_data_all_more_info <- merged_data_all_more_info %>%
  left_join(
    notrack_df %>% 
      select(domain, DomainOwnerNoTrack, CategoryNoTrack), by = "domain") #%>%
  #distinct()

#count number of TRUE in notrack_tracker
table(merged_data_all_more_info$NoTrackTracker)
table(merged_data_all_more_info$domainType)
table(merged_data_all_more_info$domainType, merged_data_all_more_info$NoTrackTracker)


## Prigent -------------------------------------------------------

merged_data_all_more_info <- merged_data_all_more_info %>%
  mutate(PrigentTracker = domain %in% prigent_df$domain)

#count number of TRUE in prigent_tracker
table(merged_data_all_more_info$PrigentTracker)
table(merged_data_all_more_info$domainType)
table(merged_data_all_more_info$domainType, merged_data_all_more_info$PrigentTracker)


## Fademind -------------------------------------------------------

merged_data_all_more_info <- merged_data_all_more_info %>%
  mutate(FademindTracker = domain %in% fademind_df$domain)

#count number of TRUE in fademind_tracker
table(merged_data_all_more_info$FademindTracker)
table(merged_data_all_more_info$domainType)
table(merged_data_all_more_info$domainType, merged_data_all_more_info$FademindTracker)


## Frogeye -------------------------------------------------------

merged_data_all_more_info <- merged_data_all_more_info %>%
  mutate(FrogeyeTracker = domain %in% frogeye_df$domain)

#count number of TRUE in frogeye_tracker
table(merged_data_all_more_info$FrogeyeTracker)
table(merged_data_all_more_info$domainType)
table(merged_data_all_more_info$domainType, merged_data_all_more_info$FrogeyeTracker)


## lockdown privacy --------------------------------------------------------

merged_data_all_more_info <- merged_data_all_more_info %>%
  mutate(LockdownPrivacyTracker = domain %in% lockdown_privacy_df$domain) %>%
  left_join(
    lockdown_privacy_df %>% 
      select(domain, CategoryLockdown), by = "domain") #%>%
  #distinct()

#count number of TRUE in lockdownprivacy_tracker
table(merged_data_all_more_info$LockdownPrivacyTracker)
table(merged_data_all_more_info$domainType)
table(merged_data_all_more_info$domainType, merged_data_all_more_info$LockdownPrivacyTracker)      


## Dan Pollock -------------------------------------------------------

merged_data_all_more_info <- merged_data_all_more_info %>%
  mutate(DanPollockTracker = domain %in% dan_pollock_df$domain)

#count number of TRUE in danpollock_tracker
table(merged_data_all_more_info$DanPollockTracker)
table(merged_data_all_more_info$domainType)
table(merged_data_all_more_info$domainType, merged_data_all_more_info$DanPollockTracker)


## AnudeepND -------------------------------------------------------

merged_data_all_more_info <- merged_data_all_more_info %>%
  mutate(AnudeepNDTracker = domain %in% anudeepND_df$domain)

#count number of TRUE in anudeepND_tracker
table(merged_data_all_more_info$AnudeepNDTracker)
table(merged_data_all_more_info$domainType)
table(merged_data_all_more_info$domainType, merged_data_all_more_info$AnudeepNDTracker)

## EasyList -------------------------------------------------------

merged_data_all_more_info <- merged_data_all_more_info %>%
  mutate(EasyListTracker = domain %in% easylist_df$domain)

#count number of TRUE in easylist_tracker
table(merged_data_all_more_info$EasyListTracker)
table(merged_data_all_more_info$domainType)
table(merged_data_all_more_info$domainType, merged_data_all_more_info$EasyListTracker)

## Developerdan -------------------------------------------------------

merged_data_all_more_info <- merged_data_all_more_info %>%
  mutate(DeveloperdanTracker = domain %in% developerdan_df$domain)

#count number of TRUE in developerdan_tracker
table(merged_data_all_more_info$DeveloperdanTracker)
table(merged_data_all_more_info$domainType)
table(merged_data_all_more_info$domainType, merged_data_all_more_info$DeveloperdanTracker)


## Yoyo -------------------------------------------------------

merged_data_all_more_info <- merged_data_all_more_info %>%
  mutate(YoyoTracker = domain %in% yoyo_df$domain)

#count number of TRUE in yoyo_tracker
table(merged_data_all_more_info$YoyoTracker)
table(merged_data_all_more_info$domainType)
table(merged_data_all_more_info$domainType, merged_data_all_more_info$YoyoTracker)


## W3C -------------------------------------------------------

merged_data_all_more_info <- merged_data_all_more_info %>%
  mutate(W3CTracker = domain %in% w3c_df$domain)

#count number of TRUE in w3c_tracker
table(merged_data_all_more_info$W3CTracker)
table(merged_data_all_more_info$domainType)
table(merged_data_all_more_info$domainType, merged_data_all_more_info$W3CTracker)


## Frogeye Multi -------------------------------------------------------

merged_data_all_more_info <- merged_data_all_more_info %>%
  mutate(FrogeyeMultiTracker = domain %in% frogeye_multi_df$domain)

#count number of TRUE in frogeye_multi_tracker
table(merged_data_all_more_info$FrogeyeMultiTracker)
table(merged_data_all_more_info$domainType)
table(merged_data_all_more_info$domainType, merged_data_all_more_info$FrogeyeMultiTracker)


## Stevenblack XL -------------------------------------------------------

merged_data_all_more_info <- merged_data_all_more_info %>%
  mutate(StevenblackXLTracker = domain %in% stevenblack_df_XL$domain)

#count number of TRUE in stevenblack_XL_tracker
table(merged_data_all_more_info$StevenblackXLTracker)
table(merged_data_all_more_info$domainType)
table(merged_data_all_more_info$domainType, merged_data_all_more_info$StevenblackXLTracker)


# 3. Cross-reference all with network activity data --------------------------

#rm(merged_data_all_trackers, merged_data_all_trackers_duplicates, merged_data_all_trackers_CategoryNoTrack_Tracker,
#   merged_data_all_trackers_unique, merged_data_all_trackers_domainType1,
#   merged_data_all_trackers_domainType2, merged_data_all_blacklist_false_domainType2)

## Blacklist ------------------------------------------------------------

## merge with blacklist df
merged_data_all_more_info <- merged_data_all_more_info %>%
  mutate(TrackerBlackList = domain %in% blacklist_df$domain)

#count number of TRUE in TrackerBlackList
table(merged_data_all_more_info$TrackerBlackList)
table(merged_data_all_more_info$domainType)
table(merged_data_all_more_info$domainType, merged_data_all_more_info$TrackerBlackList)


## Summary - with True and False Positives and False Negatives
tracker_summary <- merged_data_all_more_info %>%
  #filter(TrackerBlackList) %>%
  filter(TrackerBlackList == TRUE | domainType == 1) %>% # == TRUE or domainType == 1
  group_by(domain) %>%
  summarise(total_hits = n(), .groups = "drop") %>%
  arrange(desc(total_hits))
print(tracker_summary, n = 50)

# save as CSV
write.csv(tracker_summary, "Output/Tables/most_hit_blacklist_trackers.csv", row.names = TRUE)


## Summary with more info
#rm(tracker_summary)
tracker_summary_extended <- merged_data_all_more_info %>%
  filter(TrackerBlackList == TRUE | domainType == 1) %>% # == TRUE or domainType == 1
  group_by(domain) %>%
  summarise(total_hits = n(), .groups = "drop") %>%
  arrange(desc(total_hits)) %>%
  left_join(
    merged_data_all_more_info %>%
      select(domain, TrackerBlackList) %>%
      distinct(), by = "domain"
  ) %>%
  # add columns with per domain occurring domainTypes and apps and initiatedType
  rowwise() %>%
  mutate(
    domainType = paste(unique(merged_data_all_more_info$domainType[merged_data_all_more_info$domain == domain]), collapse = ", "),
    domainClassification = paste(unique(merged_data_all_more_info$domainClassification[merged_data_all_more_info$domain == domain]), collapse = ", "),
    hits_sum = sum(merged_data_all_more_info$hits[merged_data_all_more_info$domain == domain]),
    #hits = paste(merged_data_all_more_info$hits[merged_data_all_more_info$domain == domain], collapse = ", "),
    initiatedType = paste(unique(merged_data_all_more_info$initiatedType[merged_data_all_more_info$domain == domain]), collapse = ", "),
    DomainOwnerName = paste(unique(merged_data_all_more_info$DomainOwnerName[merged_data_all_more_info$domain == domain]), collapse = ", "),
    apps = paste(unique(merged_data_all_more_info$AppName[merged_data_all_more_info$domain == domain]), collapse = ", "),
    apps_count = length(unique(merged_data_all_more_info$AppName[merged_data_all_more_info$domain == domain])),
    CategoryNoTrack = paste(unique(merged_data_all_more_info$CategoryNoTrack[merged_data_all_more_info$domain == domain]), collapse = ", "),
    CategoryDisconnect = paste(unique(merged_data_all_more_info$CategoryDisconnect[merged_data_all_more_info$domain == domain]), collapse = ", "),
    CategoryLockdown = paste(unique(merged_data_all_more_info$CategoryLockdown[merged_data_all_more_info$domain == domain]), collapse = ", ")
  ) %>%
  ungroup()
  
print(tracker_summary_extended, n = 50)

# save as CSV
write.csv(tracker_summary_extended, "Output/Tables/most_hit_blacklist_trackers_extended.csv", row.names = TRUE)


## Create new df with only tracker entries
#rm(merged_data_all_trackers)
merged_data_all_trackers <- merged_data_all_more_info %>% # (also for True Positives and False Negatives)
  filter(TrackerBlackList == TRUE) %>%
  select(domainOwner, DomainOwnerName, DomainOwnerNoTrack, DomainOwnerMaskedD, 
         AppName, domain, domainType, TrackerBlackList, #TrackerBlackListXL, 
         CategoryNoTrack, CategoryDisconnect, OrganisationDisconnect, 
         EasyPrivacyTracker, stevenblackTracker, DisconnectTracker, MaskedDomainTracker, 
         FademindTracker, FrogeyeTracker, PrigentTracker, 
         LockdownPrivacyTracker, DanPollockTracker, AnudeepNDTracker,  EasyListTracker, YoyoTracker, 
         #DeveloperdanTracker, NoTrackTracker, W3CTracker, FrogeyeMultiTracker, StevenblackXLTracker,
         firstTimeStamp, timeStamp, hits, bundleID, initiatedType, domainClassification)

# Show duplicate trackers (over multiple apps)
#rm(merged_data_all_trackers_duplicates)
merged_data_all_trackers_duplicates <- merged_data_all_trackers %>%
  group_by(domain, AppName) %>%
  #, timeStamp, firstTimeStamp) %>%
  filter(n() > 1) %>%
  distinct(domain, AppName, .keep_all = TRUE)

# Show unique trackers
#rm(merged_data_all_trackers_unique)
merged_data_all_trackers_unique <- merged_data_all_trackers %>%
  distinct(domain, .keep_all = TRUE)


## Blacklist XL ---------------------------------------------------------

## merge with blacklist df XL
merged_data_all_more_info <- merged_data_all_more_info %>%
  mutate(TrackerBlackListXL = domain %in% blacklist_XL_df$domain)

#count number of TRUE in TrackerBlackListXL
table(merged_data_all_more_info$TrackerBlackListXL)
table(merged_data_all_more_info$domainType)
table(merged_data_all_more_info$domainType, merged_data_all_more_info$TrackerBlackListXL)


## Summary XL - with True and False Positives and False Negatives
tracker_summary_XL <- merged_data_all_more_info %>%
  filter(TrackerBlackListXL == TRUE | domainType == 1) %>% # == TRUE or domainType == 1
  #filter(TrackerBlackListXL) %>%
  group_by(domain) %>%
  summarise(total_hits = n(), .groups = "drop") %>%
  arrange(desc(total_hits))

print(tracker_summary_XL, n = 50)

# save as CSV
write.csv(tracker_summary_XL, "Output/Tables/most_hit_blacklist_XL_trackers.csv", row.names = TRUE)


## Summary with more info
#rm(tracker_summary)
tracker_summary_extended_XL <- merged_data_all_more_info %>%
  filter(TrackerBlackListXL == TRUE | domainType == 1) %>% # == TRUE or domainType == 1
  group_by(domain) %>%
  summarise(total_hits = n(), .groups = "drop") %>%
  arrange(desc(total_hits)) %>%
  left_join(
    merged_data_all_more_info %>%
      select(domain, TrackerBlackListXL, TrackerBlackList) %>%
      distinct(), by = "domain"
  ) %>%
  # add columns with per domain occuring domainTypes and apps and initiatedType
  rowwise() %>%
  mutate(
    domainType = paste(unique(merged_data_all_more_info$domainType[merged_data_all_more_info$domain == domain]), collapse = ", "),
    domainClassification = paste(unique(merged_data_all_more_info$domainClassification[merged_data_all_more_info$domain == domain]), collapse = ", "),
    hits_sum = sum(merged_data_all_more_info$hits[merged_data_all_more_info$domain == domain]),
    #hits = paste(merged_data_all_more_info$hits[merged_data_all_more_info$domain == domain], collapse = ", "),
    initiatedType = paste(unique(merged_data_all_more_info$initiatedType[merged_data_all_more_info$domain == domain]), collapse = ", "),
    DomainOwnerName = paste(unique(merged_data_all_more_info$DomainOwnerName[merged_data_all_more_info$domain == domain]), collapse = ", "),
    apps = paste(unique(merged_data_all_more_info$AppName[merged_data_all_more_info$domain == domain]), collapse = ", "),
    apps_count = length(unique(merged_data_all_more_info$AppName[merged_data_all_more_info$domain == domain])),
    CategoryNoTrack = paste(unique(merged_data_all_more_info$CategoryNoTrack[merged_data_all_more_info$domain == domain]), collapse = ", "),
    CategoryDisconnect = paste(unique(merged_data_all_more_info$CategoryDisconnect[merged_data_all_more_info$domain == domain]), collapse = ", "),
    CategoryLockdown = paste(unique(merged_data_all_more_info$CategoryLockdown[merged_data_all_more_info$domain == domain]), collapse = ", ")
  ) %>%
  ungroup()

print(tracker_summary_extended_XL, n = 50)

# save as CSV
write.csv(tracker_summary_extended_XL, "Output/Tables/most_hit_blacklist_XL_trackers_extended.csv", row.names = TRUE)


## Create new df with only tracker entries
merged_data_all_trackers_XL <- merged_data_all_more_info %>% # (also for True Positives and False Negatives)
  filter(TrackerBlackListXL == TRUE) %>%
  select(domainOwner, DomainOwnerName, DomainOwnerNoTrack, DomainOwnerMaskedD, 
         AppName, domain, domainType, TrackerBlackList, TrackerBlackListXL, 
         CategoryNoTrack, CategoryDisconnect, OrganisationDisconnect, 
         EasyPrivacyTracker, stevenblackTracker, DisconnectTracker, MaskedDomainTracker, 
         FademindTracker, FrogeyeTracker, PrigentTracker, 
         LockdownPrivacyTracker, DanPollockTracker, AnudeepNDTracker,  EasyListTracker, YoyoTracker, 
         DeveloperdanTracker, NoTrackTracker, W3CTracker, FrogeyeMultiTracker, StevenblackXLTracker,
         firstTimeStamp, timeStamp, hits, bundleID, initiatedType, domainClassification)


# Show duplicate trackers (over multiple apps)
merged_data_all_trackers_XL_duplicates <- merged_data_all_trackers_XL %>%
  group_by(domain, AppName) %>%
  #, timeStamp, firstTimeStamp) %>%
  filter(n() > 1) %>%
  distinct(domain, AppName, .keep_all = TRUE)

# Show unique trackers
merged_data_all_trackers_XL_unique <- merged_data_all_trackers_XL %>%
  distinct(domain, .keep_all = TRUE)



### Save df with more Info as CSV ###
write.csv(merged_data_all_more_info, "Output/Tables/merged_data_all_more_info_df.csv", row.names = TRUE)


## 3.1 Filtering and narrowing down --------------------------------------------

## Blacklist ---------------------------------------------------------------

#rm(merged_data_all_trackers_domainType1)
#rm(merged_data_all_trackers_domainType2)

merged_data_all_trackers_domainType1 <- merged_data_all_trackers %>%
  filter(domainType == 1) # for True Positives

merged_data_all_trackers_domainType2 <- merged_data_all_trackers %>%
  filter(domainType == 2) # for False Negatives

merged_data_all_blacklist_false_domainType2 <- merged_data_all_more_info %>%
  select(DomainOwnerName, AppName, domain, domainType, TrackerBlackList, #TrackerBlackListXL, 
         firstTimeStamp, timeStamp, hits, initiatedType, domainClassification) %>%
  filter(TrackerBlackList == FALSE & domainType == 2) # for True Negatives

merged_data_all_blacklist_false_domainType1 <- merged_data_all_more_info %>%
  select(DomainOwnerName, AppName, domain, domainType, TrackerBlackList, #TrackerBlackListXL, 
         firstTimeStamp, timeStamp, hits, initiatedType, domainClassification) %>%
  filter(TrackerBlackList == FALSE & domainType == 1) # for False Positives


## print table with number of True and False Positives and Negatives
confusion_matrix <- table(merged_data_all_more_info$domainType, merged_data_all_more_info$TrackerBlackList)
confusion_matrix <- addmargins(confusion_matrix, FUN = list(Total = sum))
print(confusion_matrix)

# write confusion matrix to CSV
write.csv(as.data.frame(confusion_matrix), "Output/Tables/blacklist_domainType1_confusion_matrix.csv", row.names = TRUE)


#rm(merged_data_all_trackers_CategoryNoTrack_Tracker)
#merged_data_all_trackers_CategoryNoTrack_Tracker <- merged_data_all_trackers %>%
#  filter(CategoryNoTrack != "" & !is.na(CategoryNoTrack))

## Blacklist XL ------------------------------------------------------------

#rm(merged_data_all_trackers_domainType1)
#rm(merged_data_all_trackers_domainType2)

merged_data_all_trackers_XL_domainType1 <- merged_data_all_trackers_XL %>%
  filter(domainType == 1) # for True Positives

merged_data_all_trackers_XL_domainType2 <- merged_data_all_trackers_XL %>%
  filter(domainType == 2) # for False Negatives

merged_data_all_blacklist_XL_false_domainType2 <- merged_data_all_more_info %>%
  select(DomainOwnerName, AppName, domain, domainType, TrackerBlackList, TrackerBlackListXL, 
         firstTimeStamp, timeStamp, hits, initiatedType, domainClassification) %>%
  filter(TrackerBlackListXL == FALSE & domainType == 2) # for True Negatives

merged_data_all_blacklist_XL_false_domainType1 <- merged_data_all_more_info %>%
  select(DomainOwnerName, AppName, domain, domainType, TrackerBlackList, TrackerBlackListXL, 
         firstTimeStamp, timeStamp, hits, initiatedType, domainClassification) %>%
  filter(TrackerBlackListXL == FALSE & domainType == 1) # for False Positives


## print table with number of True and False Positives and Negatives
confusion_matrix_XL <- table(merged_data_all_more_info$domainType, merged_data_all_more_info$TrackerBlackListXL)
confusion_matrix_XL <- addmargins(confusion_matrix_XL, FUN = list(Total = sum))
print(confusion_matrix_XL)

# write confusion matrix to CSV
write.csv(as.data.frame(confusion_matrix_XL), "Output/Tables/blacklist_XL_domainType1_confusion_matrix.csv", row.names = TRUE)


## Opposite Df's -----------------------------------------------------------

# Opposite df's
#rm(merged_data_all_domainType1, merged_data_all_domainType2)
merged_data_all_domainType1 <- merged_data_all_more_info %>%
  select(DomainOwnerName, AppName, domain, domainType, TrackerBlackList, TrackerBlackListXL, 
         firstTimeStamp, timeStamp, hits, initiatedType, domainClassification) %>%
  filter(domainType == 1) # for True and False Positives

merged_data_all_domainType2 <- merged_data_all_more_info %>%
  select(DomainOwnerName, AppName, domain, domainType, TrackerBlackList, TrackerBlackListXL, 
         firstTimeStamp, timeStamp, hits, initiatedType, domainClassification) %>%
  filter(domainType == 2) # for True and False Negatives


# 4. CT OFF - New Columns for domainOwner and Cross-Ref ---------------

# 4.1 DomainOwner -------------------------------------------------------------

# domain owner
merged_data_ct_off %>%
  group_by(domainOwner) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=76)

# number of unique domainOwner
length(unique(merged_data_ct_off$domainOwner))

# create new data frame with new column "DomainOwnerName" that renames all the domains that contain strings like "google", "apple", "facebook", "amazon", to that string
library(tidyverse) # For data manipulation


## Creating new column DomainOwnerName ---------------------------------------------------------

merged_data_ct_off <- merged_data_ct_off %>%
  mutate(DomainOwnerName = case_when(
    str_detect(domainOwner, "Microsoft Corporation") ~ "Microsoft",
    str_detect(domainOwner, "Adobe Inc.", ) ~ "Adobe",
    str_detect(domainOwner, "AddApptr GmbH") ~ "AddApptr",
    str_detect(domainOwner, "Google LLC") ~ "Google",
    str_detect(domainOwner, "Facebook, Inc.") ~ "Meta",
    str_detect(domainOwner, "Unity Software Inc.") ~ "Unity Inc.",
    # additional DomainOwners
    str_detect(domainOwner, "Braze, Inc.") ~ "Braze Inc.",
    str_detect(domainOwner, "Urban Airship, Inc.") ~ "United Airship Inc.",
    str_detect(domainOwner, "SAP SE") ~ "SAP SE",
    str_detect(domainOwner, "Kochava") ~ "Kochava",
    str_detect(domainOwner, "AppLovin Corporation") ~ "AppLovin Corp.",
    str_detect(domainOwner, "RevenueCat") ~ "RevenueCat",
    str_detect(domainOwner, "Vungle Inc") ~ "Vungle Inc.",
    str_detect(domainOwner, "Amplitude") ~ "Amplitude",
    str_detect(domainOwner, "Digital Turbine") ~ "Digital Turbine",
    str_detect(domainOwner, "Snap Inc.") ~ "Snap Inc.",
    str_detect(domainOwner, "Amazon Technologies, Inc.") ~ "Amazon",
    str_detect(domainOwner, "AppsFlyer") ~ "AppsFlyer",
    str_detect(domainOwner, "Twitter, Inc.") ~ "Twitter Inc.",
    str_detect(domainOwner, "ByteDance Ltd.") ~ "ByteDance Ltd.",
    str_detect(domainOwner, "CleverTap") ~ "CleverTap",
    str_detect(domainOwner, "Moloco Inc.") ~ "Moloco Inc.",
    str_detect(domainOwner, "Criteo SA") ~ "Criteo SA",
    str_detect(domainOwner, "DataDome") ~ "DataDome",
    str_detect(domainOwner, "Forter Inc.") ~ "Forter Inc.",
    str_detect(domainOwner, "Bugsnag Inc.") ~ "Bugsnag Inc.",
    str_detect(domainOwner, "Datadog, Inc.") ~ "Datadog Inc.",
    str_detect(domainOwner, "InMobi Pte Ltd") ~ "InMobi Pte",
    str_detect(domainOwner, "Index Exchange, Inc.") ~ "Index Exchange",
    str_detect(domainOwner, "PayPal, Inc.") ~ "PayPal",
    str_detect(domainOwner, "Reddit Inc.") ~ "Reddit",
    str_detect(domainOwner, "OneSignal") ~ "OneSignal",
    str_detect(domainOwner, "OpenX Technologies Inc") ~ "OpenX Inc.",
    str_detect(domainOwner, "YieldMo, Inc.") ~ "YieldMo Inc.",
    str_detect(domainOwner, "Alibaba Group") ~ "Alibaba Group",
    str_detect(domainOwner, "IPONWEB GmbH") ~ "IPONWEB",
    str_detect(domainOwner, "Magnite, Inc.") ~ "Magnite Inc.",
    str_detect(domainOwner, "Mixpanel, Inc.") ~ "Mixpanel Inc.",
    str_detect(domainOwner, "The Trade Desk Inc") ~ "Trade Desk Inc.",
    str_detect(domainOwner, "TripleLift") ~ "TripleLift",
    str_detect(domainOwner, "Prospect One") ~ "Prospect One",
    str_detect(domainOwner, "Segment.io, Inc.") ~ "Segment Inc.",
    str_detect(domainOwner, "Vizbee, Inc.") ~ "Vizbee Inc.",
    str_detect(domainOwner, "comScore, Inc") ~ "comScore Inc.",
    str_detect(domainOwner, "Cloudflare, Inc.") ~ "Cloudflare",
    str_detect(domainOwner, "Iterable, Inc.") ~ "Iterable",
    str_detect(domainOwner, "LiveRamp") ~ "LiveRamp",
    str_detect(domainOwner, "Mobvista") ~ "Mobvista",
    str_detect(domainOwner, "Pulsepoint, Inc.") ~ "Pulsepoint",
    str_detect(domainOwner, "Salesforce.com, Inc.") ~ "Salesforce",
    str_detect(domainOwner, "ShareThis, Inc") ~ "ShareThis Inc.",
    str_detect(domainOwner, "Sift Science, Inc.") ~ "Sift Science Inc.",
    str_detect(domainOwner, "Verizon Media") ~ "Verizon Media",
    str_detect(domainOwner, "Zeta Global") ~ "Zeta Global",
    str_detect(domainOwner, "Fastly, Inc.") ~ "Fastly",
    str_detect(domainOwner, "GumGum") ~ "GumGum",
    str_detect(domainOwner, "Helpshift, Inc.") ~ "Helpshift",
    str_detect(domainOwner, "ID5 Technology Ltd") ~ "ID5 Technology",
    str_detect(domainOwner, "LiveIntent Inc.") ~ "LiveIntent Inc.",
    str_detect(domainOwner, "Lotame Solutions, Inc.") ~ "Lotame Solutions",
    str_detect(domainOwner, "Propeller Ads") ~ "Propeller Ads",
    str_detect(domainOwner, "PubMatic, Inc.") ~ "PubMatic Inc.",
    str_detect(domainOwner, "Quantcast Corporation") ~ "Quantcast Corp.",
    str_detect(domainOwner, "Sharethrough, Inc.") ~ "Sharethrough Inc.",
    str_detect(domainOwner, "Singular Labs, Inc.") ~ "Singular Labs",
    str_detect(domainOwner, "Sovrn Holdings") ~ "Sovrn Holdings",
    str_detect(domainOwner, "Teads \\( Luxenbourg \\) SA") ~ "Teads",
    str_detect(domainOwner, "The Rubicon Project, Inc.") ~ "Rubicon Project",
    str_detect(domainOwner, "Throtle") ~ "Throtle",
    str_detect(domainOwner, "Trustpilot A/S") ~ "Trustpilot",
    str_detect(domainOwner, "Undertone Networks") ~ "Undertone Networks",
    str_detect(domainOwner, "WarnerMedia, LLC") ~ "WarnerMedia",
    str_detect(domainOwner, "Wingify") ~ "Wingify",
    str_detect(domainOwner, "Yieldlove GmbH") ~ "Yieldlove",
    
    # adding via domains
    str_detect(domain, "google") ~ "Google",
    str_detect(domain, "youtube") ~ "Google", 
    str_detect(domain, "gstatic") ~ "Google",
    str_detect(domain, "googleapis") ~ "Google",
    str_detect(domain, "app-analytics-services.com") ~ "Google",
    str_detect(domain, "firebase-settings.crashlytics.com") ~ "Google",
    str_detect(domain, "firebaselogging-pa.googleapis.com") ~ "Google",
    str_detect(domain, "g.doubleclick") ~ "Google",
    str_detect(domain, "ad.doubleclick.net") ~ "Google",
    str_detect(domain, "apple") ~ "Apple",
    str_detect(domain, "mzstatic") ~ "Apple",
    str_detect(domain, "icloud") ~ "Apple",
    str_detect(domain, "itunes") ~ "Apple",
    str_detect(domain, "facebook") ~ "Meta",
    str_detect(domain, "whatsapp") ~ "Meta",
    str_detect(domain, "instagram") ~ "Meta",
    str_detect(domain, "threads") ~ "Meta",
    str_detect(domain, "fbcdn") ~ "Meta",
    str_detect(domain, "giphy") ~ "Shutterstock",
    #str_detect(domain, "messenger") ~ "Meta",
    str_detect(domain, "amazon") ~ "Amazon",
    str_detect(domain, "microsoft") ~ "Microsoft",
    str_detect(domain, "linkedin") ~ "Microsoft",
    str_detect(domain, "bing") ~ "Microsoft",
    str_detect(domain, "skype") ~ "Microsoft",
    str_detect(domain, "adobe") ~ "Adobe",
    str_detect(domain, "appsflyer") ~ "AppsFlyer",
    str_detect(domain, "unity3d") ~ "Unity Software",
    str_detect(domain, "unity.com") ~ "Unity Software",
    str_detect(domain, "ingest.sentry.io") ~ "Functional Software",
    str_detect(domain, "twitter") ~ "Twitter Inc.",
    str_detect(domain, "tiktok") ~ "ByteDance Ltd.",
    str_detect(domain, "mozilla") ~ "Mozilla Firefox",
    str_detect(domain, "spotify") ~ "Spotify",
    str_detect(domain, "reddit") ~ "Reddit",
    str_detect(domain, "snapchat") ~ "Snap Inc.",
    str_detect(domain, "tutti") ~ "SMG AG",
    str_detect(domain, "ricardo") ~ "SMG AG",
    str_detect(domain, "jodel") ~ "Jodel Venture GmbH",
    str_detect(domain, "strava") ~ "Strava Inc.",
    str_detect(domain, "signal.org") ~ "Signal Technology Foundation",
    str_detect(domain, "brave.com") ~ "Brave Software Inc.",
    str_detect(domain, "spiegel") ~ "Rudolf Augstein GmbH & Co. KG",
    str_detect(domain, "aliexpress") ~ "Alibaba Group",
    str_detect(domain, "alibaba") ~ "Alibaba Group",
    str_detect(domain, "alipay") ~ "Alibaba Group",
    str_detect(domain, "alicdn") ~ "Alibaba Group",
    str_detect(domain, "aliapp") ~ "Alibaba Group",
    str_detect(domain, "qq.com") ~ "Tencent Holdings Limited",
    str_detect(domain, "wechat.com") ~ "Tencent Holdings Limited",
    str_detect(domain, "tencent") ~ "Tencent Holdings Limited",
    str_detect(domain, "twint") ~ "TWINT AG",
    str_detect(domain, "20min") ~ "TX Group AG",
    str_detect(domain, "migro") ~ "Migros-Genossenschafts-Bund (MGB)",
    str_detect(domain, "srgssr") ~ "SRG SRF",
    str_detect(domain, "digitec") ~ "Digitec Galaxus AG",
    str_detect(domain, "galaxus") ~ "Digitec Galaxus AG",
    str_detect(domain, "sbb.ch") ~ "SBB CFF FFS",
    str_detect(domain, "adnxs") ~ "Microsoft",
    str_detect(domain, "doubleclick") ~ "Google",
    str_detect(domain, "bsky") ~ "Bluesky Social PBC",
    str_detect(domain, "doubleverify") ~ "DoubleVerify Holdings Inc.",
    str_detect(domain, "lencr") ~ "Let's Encrypt",
    str_detect(domain, "onesignal") ~ "OneSignal",
    str_detect(domain, "api3.branch.io") ~ "Branch",
    str_detect(domain, "app.usercentrics.eu") ~ "Usercentrics GmbH",
    str_detect(domain, "cacerts.digicert.com") ~ "DigiCert",
    str_detect(domain, "cdn.branch.io") ~ "Branch",
    str_detect(domain, "cdn.cookielaw.org") ~ "OneTrust",
    str_detect(domain, "census-app-x.scorecardresearch.com") ~ "comScore Inc.",
    str_detect(domain, "config.mapbox.com") ~ "Mapbox",
    str_detect(domain, "ocsp.digicert.com") ~ "DigiCert",
    str_detect(domain, "outlook.office365.com") ~ "Microsoft",
    str_detect(domain, "prod-mediate-events.applovin.com") ~ "AppLovin Corp.",
    str_detect(domain, "region1.app-analytics-services-att.com") ~ "AT&T Inc.",
    str_detect(domain, "static.zdassets.com") ~ "Zendesk",
    
    
    TRUE ~ "Other"
  ))

merged_data_ct_off <- merged_data_ct_off %>% 
  select(firstTimeStamp, timeStamp, hits, bundleID, AppName, domain, domainOwner, DomainOwnerName, everything()) 

# Save df as CSV
write.csv(merged_data_ct_off, "Output/Tables/merged_data_ct_off_df.csv", row.names = TRUE)

## Printing DomainOwnerName summaries ------------------------------------------------------
library(dplyr)
merged_data_ct_off %>%
  group_by(DomainOwnerName) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=104) %>%
  # add new empty "notes" column
  mutate(notes = case_when(
    TRUE ~ "" # Default case for all other domains
  )) %>%
  write.csv("Output/Tables/DomainOwnerName_summary_ct_off.csv", row.names = TRUE)


merged_data_ct_off %>%
  group_by(domain) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=150) 

## Filtered Df domain > 5 -------------------------------------------------------------

# New df with domains that have more than 5 accesses
merged_data_ct_off_domain_10plus <- merged_data_ct_off %>%
  group_by(domain) %>%
  summarise(total_accesses = n()) %>%
  filter(total_accesses >= 5) %>%
  arrange(desc(total_accesses)) %>%
  # add DomainOwnerName and domainType columns
  left_join(merged_data_ct_off %>% 
              select(domain, domainOwner, DomainOwnerName, domainType, domainClassification, initiatedType) %>%
              distinct(), by = "domain") %>%
  # add new empty "notes" column
  mutate(notes = case_when(
    TRUE ~ "" # Default case for all other domains
  )) %>%
  print(n=150)

# Save df as CSV
write.csv(merged_data_ct_off_domain_10plus, "Output/Tables/most_accessed_domains_10plus_ct_off.csv", row.names = TRUE)


# New df with domains that have more than 10 accesses and all selected columns
merged_data_ct_off_domain_10plus_full <- merged_data_ct_off %>%
  filter(domain %in% merged_data_ct_off_domain_10plus$domain) %>%
  select(firstTimeStamp, timeStamp, hits, bundleID, AppName, domain, domainOwner, DomainOwnerName, everything()) %>%
  arrange(desc(timeStamp)) 

# Save df as CSV
write.csv(merged_data_ct_off_domain_10plus_full, "Output/Tables/most_accessed_domains_10plus_full_ct_off.csv", row.names = TRUE)


## New df's with domain Owners ---------------------------------------

# New df for domain Owner Names with domains that have more than 10 accesses
merged_data_ct_off_domain_10plus_domainOwners <- merged_data_ct_off_domain_10plus %>%
  #group_by(domainOwner) %>%
  group_by(DomainOwnerName) %>%
  summarise(total_accesses = n()) %>%
  #filter(total_accesses > 10) %>%
  arrange(desc(total_accesses)) %>%
  mutate(notes = case_when(   # add new empty "notes" column
    TRUE ~ "" # Default case for all other domains
  )) %>%
  print(n=150)

write.csv(merged_data_ct_off_domain_10plus_domainOwners, "Output/Tables/DomainOwnerName_summary_10plus_ct_off.csv", row.names = TRUE)

# Count domainType
table(merged_data_ct_off$domainType)
table(merged_data_ct_off_domain_10plus$domainType)

5582/100
682/55.82

# unique number of domains where domainType = 1
length(unique(merged_data_ct_off$domain[merged_data_ct_off$domainType == 1])) # 136

length(unique(merged_data_ct_off$domain[merged_data_ct_off$domainType == 2])) # 2168

# unique number of bundleIDs
length(unique(merged_data_ct_off$bundleID)) # 122


# 4.2 Adding tracker info to merged_data_ct_off_more_info -----------------------

library(dplyr)

# remove leading www. from domain in merged_data_ct_off
merged_data_ct_off <- merged_data_ct_off %>%
  mutate(domain = str_remove(domain, "^www\\."))

#rm(merged_data_ct_off_more_info)
merged_data_ct_off_more_info <- merged_data_ct_off


## Disconnect ----------------------------------------------------

merged_data_ct_off_more_info <- merged_data_ct_off_more_info %>%
  mutate(DisconnectTracker = domain %in% disconnect_df$domain)

merged_data_ct_off_more_info <- merged_data_ct_off_more_info %>%
  left_join(
    disconnect_df %>% 
      select(domain, OrganisationDisconnect, CategoryDisconnect), by = "domain") #%>%
#distinct()

#count number of TRUE in disconnect_tracker
table(merged_data_ct_off_more_info$DisconnectTracker)
table(merged_data_ct_off_more_info$domainType)
table(merged_data_ct_off_more_info$domainType, merged_data_ct_off_more_info$DisconnectTracker)


## EasyPrivacy -----------------------------

library(dplyr)
# find all network activity entries that match EasyList EasyPrivacy and add a tracker flag 
merged_data_ct_off_more_info <- merged_data_ct_off_more_info %>%
  mutate(EasyPrivacyTracker = domain %in% easyprivacy_df$domain)

#count number of TRUE in easylist_tracker
table(merged_data_ct_off_more_info$EasyPrivacyTracker)
table(merged_data_ct_off_more_info$domainType)
table(merged_data_ct_off_more_info$domainType, merged_data_ct_off_more_info$EasyPrivacyTracker)


## Summary
#tracker_summary_easylist <- merged_data_ct_off_more_info %>%
#filter(EasyPrivacyTracker) %>%
#group_by(domain) %>%
#summarise(total_hits = n(), .groups = "drop") %>%
#arrange(desc(total_hits))

#print(tracker_summary_easylist, n = 50)


## Stevenblack ---------------------------------------------------

merged_data_ct_off_more_info <- merged_data_ct_off_more_info %>%
  mutate(stevenblackTracker = domain %in% stevenblack_df$domain)

#count number of TRUE in easylist_tracker
table(merged_data_ct_off_more_info$stevenblackTracker)
table(merged_data_ct_off_more_info$domainType)
table(merged_data_ct_off_more_info$domainType, merged_data_ct_off_more_info$stevenblackTracker)


## Masked Domain -------------------------------------------------

merged_data_ct_off_more_info <- merged_data_ct_off_more_info %>%
  mutate(MaskedDomainTracker = domain %in% masked_domain_df$domain) %>%
  # add DomainOwnerMaskedD info column
  left_join(masked_domain_df %>% 
              select(domain, DomainOwnerMaskedD), by = "domain") #%>%
#distinct()

#count number of TRUE in masked_domain_tracker
table(merged_data_ct_off_more_info$MaskedDomainTracker)
table(merged_data_ct_off_more_info$domainType)
table(merged_data_ct_off_more_info$domainType, merged_data_ct_off_more_info$MaskedDomainTracker)


## NoTrack -------------------------------------------------------

merged_data_ct_off_more_info <- merged_data_ct_off_more_info %>%
  mutate(NoTrackTracker = domain %in% notrack_df$domain)

merged_data_ct_off_more_info <- merged_data_ct_off_more_info %>%
  left_join(
    notrack_df %>% 
      select(domain, DomainOwnerNoTrack, CategoryNoTrack), by = "domain") #%>%
#distinct()

#count number of TRUE in notrack_tracker
table(merged_data_ct_off_more_info$NoTrackTracker)
table(merged_data_ct_off_more_info$domainType)
table(merged_data_ct_off_more_info$domainType, merged_data_ct_off_more_info$NoTrackTracker)


## Prigent -------------------------------------------------------

merged_data_ct_off_more_info <- merged_data_ct_off_more_info %>%
  mutate(PrigentTracker = domain %in% prigent_df$domain)

#count number of TRUE in prigent_tracker
table(merged_data_ct_off_more_info$PrigentTracker)
table(merged_data_ct_off_more_info$domainType)
table(merged_data_ct_off_more_info$domainType, merged_data_ct_off_more_info$PrigentTracker)


## Fademind -------------------------------------------------------

merged_data_ct_off_more_info <- merged_data_ct_off_more_info %>%
  mutate(FademindTracker = domain %in% fademind_df$domain)

#count number of TRUE in fademind_tracker
table(merged_data_ct_off_more_info$FademindTracker)
table(merged_data_ct_off_more_info$domainType)
table(merged_data_ct_off_more_info$domainType, merged_data_ct_off_more_info$FademindTracker)


## Frogeye -------------------------------------------------------

merged_data_ct_off_more_info <- merged_data_ct_off_more_info %>%
  mutate(FrogeyeTracker = domain %in% frogeye_df$domain)

#count number of TRUE in frogeye_tracker
table(merged_data_ct_off_more_info$FrogeyeTracker)
table(merged_data_ct_off_more_info$domainType)
table(merged_data_ct_off_more_info$domainType, merged_data_ct_off_more_info$FrogeyeTracker)


## lockdown privacy --------------------------------------------------------

merged_data_ct_off_more_info <- merged_data_ct_off_more_info %>%
  mutate(LockdownPrivacyTracker = domain %in% lockdown_privacy_df$domain) %>%
  left_join(
    lockdown_privacy_df %>% 
      select(domain, CategoryLockdown), by = "domain") #%>%
#distinct()

#count number of TRUE in lockdownprivacy_tracker
table(merged_data_ct_off_more_info$LockdownPrivacyTracker)
table(merged_data_ct_off_more_info$domainType)
table(merged_data_ct_off_more_info$domainType, merged_data_ct_off_more_info$LockdownPrivacyTracker)      


## Dan Pollock -------------------------------------------------------

merged_data_ct_off_more_info <- merged_data_ct_off_more_info %>%
  mutate(DanPollockTracker = domain %in% dan_pollock_df$domain)

#count number of TRUE in danpollock_tracker
table(merged_data_ct_off_more_info$DanPollockTracker)
table(merged_data_ct_off_more_info$domainType)
table(merged_data_ct_off_more_info$domainType, merged_data_ct_off_more_info$DanPollockTracker)


## AnudeepND -------------------------------------------------------

merged_data_ct_off_more_info <- merged_data_ct_off_more_info %>%
  mutate(AnudeepNDTracker = domain %in% anudeepND_df$domain)

#count number of TRUE in anudeepND_tracker
table(merged_data_ct_off_more_info$AnudeepNDTracker)
table(merged_data_ct_off_more_info$domainType)
table(merged_data_ct_off_more_info$domainType, merged_data_ct_off_more_info$AnudeepNDTracker)

## EasyList -------------------------------------------------------

merged_data_ct_off_more_info <- merged_data_ct_off_more_info %>%
  mutate(EasyListTracker = domain %in% easylist_df$domain)

#count number of TRUE in easylist_tracker
table(merged_data_ct_off_more_info$EasyListTracker)
table(merged_data_ct_off_more_info$domainType)
table(merged_data_ct_off_more_info$domainType, merged_data_ct_off_more_info$EasyListTracker)

## Developerdan -------------------------------------------------------

merged_data_ct_off_more_info <- merged_data_ct_off_more_info %>%
  mutate(DeveloperdanTracker = domain %in% developerdan_df$domain)

#count number of TRUE in developerdan_tracker
table(merged_data_ct_off_more_info$DeveloperdanTracker)
table(merged_data_ct_off_more_info$domainType)
table(merged_data_ct_off_more_info$domainType, merged_data_ct_off_more_info$DeveloperdanTracker)


## Yoyo -------------------------------------------------------

merged_data_ct_off_more_info <- merged_data_ct_off_more_info %>%
  mutate(YoyoTracker = domain %in% yoyo_df$domain)

#count number of TRUE in yoyo_tracker
table(merged_data_ct_off_more_info$YoyoTracker)
table(merged_data_ct_off_more_info$domainType)
table(merged_data_ct_off_more_info$domainType, merged_data_ct_off_more_info$YoyoTracker)


## W3C -------------------------------------------------------

merged_data_ct_off_more_info <- merged_data_ct_off_more_info %>%
  mutate(W3CTracker = domain %in% w3c_df$domain)

#count number of TRUE in w3c_tracker
table(merged_data_ct_off_more_info$W3CTracker)
table(merged_data_ct_off_more_info$domainType)
table(merged_data_ct_off_more_info$domainType, merged_data_ct_off_more_info$W3CTracker)


## Frogeye Multi -------------------------------------------------------

merged_data_ct_off_more_info <- merged_data_ct_off_more_info %>%
  mutate(FrogeyeMultiTracker = domain %in% frogeye_multi_df$domain)

#count number of TRUE in frogeye_multi_tracker
table(merged_data_ct_off_more_info$FrogeyeMultiTracker)
table(merged_data_ct_off_more_info$domainType)
table(merged_data_ct_off_more_info$domainType, merged_data_ct_off_more_info$FrogeyeMultiTracker)


## Stevenblack XL -------------------------------------------------------

merged_data_ct_off_more_info <- merged_data_ct_off_more_info %>%
  mutate(StevenblackXLTracker = domain %in% stevenblack_df_XL$domain)

#count number of TRUE in stevenblack_XL_tracker
table(merged_data_ct_off_more_info$StevenblackXLTracker)
table(merged_data_ct_off_more_info$domainType)
table(merged_data_ct_off_more_info$domainType, merged_data_ct_off_more_info$StevenblackXLTracker)


# 4.3 Cross-reference all with network activity data --------------------------

## Blacklist ------------------------------------------------------------

## merge with blacklist df
merged_data_ct_off_more_info <- merged_data_ct_off_more_info %>%
  mutate(TrackerBlackList = domain %in% blacklist_df$domain)

#count number of TRUE in TrackerBlackList
table(merged_data_ct_off_more_info$TrackerBlackList)
table(merged_data_ct_off_more_info$domainType)
table(merged_data_ct_off_more_info$domainType, merged_data_ct_off_more_info$TrackerBlackList)


## Summary - with True and False Positives and False Negatives
tracker_summary_ct_off <- merged_data_ct_off_more_info %>%
  filter(TrackerBlackList == TRUE | domainType == 1) %>% # == TRUE or domainType == 1
  #filter(TrackerBlackList) %>%
  group_by(domain) %>%
  summarise(total_hits = n(), .groups = "drop") %>%
  arrange(desc(total_hits))

print(tracker_summary_ct_off, n = 50)

# save as CSV
write.csv(tracker_summary_ct_off, "Output/Tables/most_hit_blacklist_trackers_ct_off.csv", row.names = TRUE)


## Summary with more info
#rm(tracker_summary)
tracker_summary_extended_ct_off <- merged_data_ct_off_more_info %>%
  filter(TrackerBlackList == TRUE | domainType == 1) %>% # == TRUE or domainType == 1
  group_by(domain) %>%
  summarise(total_hits = n(), .groups = "drop") %>%
  arrange(desc(total_hits)) %>%
  left_join(
    merged_data_ct_off_more_info %>%
      select(domain, TrackerBlackList) %>%
      distinct(), by = "domain"
  ) %>%
  # add columns with per domain occurring domainTypes and apps and initiatedType
  rowwise() %>%
  mutate(
    domainType = paste(unique(merged_data_ct_off_more_info$domainType[merged_data_ct_off_more_info$domain == domain]), collapse = ", "),
    domainClassification = paste(unique(merged_data_ct_off_more_info$domainClassification[merged_data_ct_off_more_info$domain == domain]), collapse = ", "),
    hits_sum = sum(merged_data_ct_off_more_info$hits[merged_data_ct_off_more_info$domain == domain]),
    #hits = paste(merged_data_ct_off_more_info$hits[merged_data_ct_off_more_info$domain == domain], collapse = ", "),
    initiatedType = paste(unique(merged_data_ct_off_more_info$initiatedType[merged_data_ct_off_more_info$domain == domain]), collapse = ", "),
    DomainOwnerName = paste(unique(merged_data_ct_off_more_info$DomainOwnerName[merged_data_ct_off_more_info$domain == domain]), collapse = ", "),
    apps = paste(unique(merged_data_ct_off_more_info$AppName[merged_data_ct_off_more_info$domain == domain]), collapse = ", "),
    apps_count = length(unique(merged_data_ct_off_more_info$AppName[merged_data_ct_off_more_info$domain == domain])),
    CategoryNoTrack = paste(unique(merged_data_ct_off_more_info$CategoryNoTrack[merged_data_ct_off_more_info$domain == domain]), collapse = ", "),
    CategoryDisconnect = paste(unique(merged_data_ct_off_more_info$CategoryDisconnect[merged_data_ct_off_more_info$domain == domain]), collapse = ", "),
    CategoryLockdown = paste(unique(merged_data_ct_off_more_info$CategoryLockdown[merged_data_ct_off_more_info$domain == domain]), collapse = ", ")
  ) %>%
  ungroup()

print(tracker_summary_extended_ct_off, n = 50)

# save as CSV
write.csv(tracker_summary_extended_ct_off, "Output/Tables/most_hit_blacklist_trackers_extended_ct_off.csv", row.names = TRUE)



## Create new df with only tracker entries
#rm(merged_data_all_trackers)
merged_data_ct_off_trackers <- merged_data_ct_off_more_info %>% # (also for True Positives and False Negatives)
  filter(TrackerBlackList == TRUE) %>%
  select(domainOwner, DomainOwnerName, DomainOwnerNoTrack, DomainOwnerMaskedD, 
         AppName, domain, domainType, TrackerBlackList, #TrackerBlackListXL, 
         CategoryNoTrack, CategoryDisconnect, OrganisationDisconnect, 
         EasyPrivacyTracker, stevenblackTracker, DisconnectTracker, MaskedDomainTracker, 
         FademindTracker, FrogeyeTracker, PrigentTracker, 
         LockdownPrivacyTracker, DanPollockTracker, AnudeepNDTracker,  EasyListTracker, YoyoTracker, 
         #DeveloperdanTracker, NoTrackTracker, W3CTracker, FrogeyeMultiTracker, StevenblackXLTracker,
         firstTimeStamp, timeStamp, hits, bundleID, initiatedType, domainClassification)


# Show duplicate trackers (over multiple apps)
#rm(merged_data_all_trackers_duplicates)
merged_data_ct_off_trackers_duplicates <- merged_data_ct_off_trackers %>%
  group_by(domain, AppName) %>%
  #, timeStamp, firstTimeStamp) %>%
  filter(n() > 1) %>%
  distinct(domain, AppName, .keep_all = TRUE)

# Show unique trackers
#rm(merged_data_all_trackers_unique)
merged_data_ct_off_trackers_unique <- merged_data_ct_off_trackers %>%
  distinct(domain, .keep_all = TRUE)


### Save df with more Info as CSV ###
write.csv(merged_data_ct_off_more_info, "Output/Tables/merged_data_ct_off_more_info_df.csv", row.names = TRUE)


## Blacklist XL ---------------------------------------------------------

## merge with blacklist df XL
merged_data_ct_off_more_info <- merged_data_ct_off_more_info %>%
  mutate(TrackerBlackListXL = domain %in% blacklist_XL_df$domain)

#count number of TRUE in TrackerBlackListXL
table(merged_data_ct_off_more_info$TrackerBlackListXL)
table(merged_data_ct_off_more_info$domainType)
table(merged_data_ct_off_more_info$domainType, merged_data_ct_off_more_info$TrackerBlackListXL)


## Summary XL - with True and False Positives and False Negatives
tracker_summary_XL_ct_off <- merged_data_ct_off_more_info %>%
  filter(TrackerBlackListXL == TRUE | domainType == 1) %>% # == TRUE or domainType == 1
  #filter(TrackerBlackList) %>%
  group_by(domain) %>%
  summarise(total_hits = n(), .groups = "drop") %>%
  arrange(desc(total_hits))

print(tracker_summary_XL_ct_off, n = 50)

# save as CSV
write.csv(tracker_summary_XL_ct_off, "Output/Tables/most_hit_blacklist_trackers_XL_ct_off.csv", row.names = TRUE)


## Summary XL - with more info
#rm(tracker_summary_XL_extended_ct_off)
tracker_summary_XL_extended_ct_off <- merged_data_ct_off_more_info %>%
  filter(TrackerBlackListXL == TRUE | domainType == 1) %>% # == TRUE or domainType == 1
  group_by(domain) %>%
  summarise(total_hits = n(), .groups = "drop") %>%
  arrange(desc(total_hits)) %>%
  left_join(
    merged_data_ct_off_more_info %>%
      select(domain, TrackerBlackList) %>%
      distinct(), by = "domain"
  ) %>%
  # add columns with per domain occurring domainTypes and apps and initiatedType
  rowwise() %>%
  mutate(
    domainType = paste(unique(merged_data_ct_off_more_info$domainType[merged_data_ct_off_more_info$domain == domain]), collapse = ", "),
    domainClassification = paste(unique(merged_data_ct_off_more_info$domainClassification[merged_data_ct_off_more_info$domain == domain]), collapse = ", "),
    hits_sum = sum(merged_data_ct_off_more_info$hits[merged_data_ct_off_more_info$domain == domain]),
    #hits = paste(merged_data_ct_off_more_info$hits[merged_data_ct_off_more_info$domain == domain], collapse = ", "),
    initiatedType = paste(unique(merged_data_ct_off_more_info$initiatedType[merged_data_ct_off_more_info$domain == domain]), collapse = ", "),
    DomainOwnerName = paste(unique(merged_data_ct_off_more_info$DomainOwnerName[merged_data_ct_off_more_info$domain == domain]), collapse = ", "),
    apps = paste(unique(merged_data_ct_off_more_info$AppName[merged_data_ct_off_more_info$domain == domain]), collapse = ", "),
    apps_count = length(unique(merged_data_ct_off_more_info$AppName[merged_data_ct_off_more_info$domain == domain])),
    CategoryNoTrack = paste(unique(merged_data_ct_off_more_info$CategoryNoTrack[merged_data_ct_off_more_info$domain == domain]), collapse = ", "),
    CategoryDisconnect = paste(unique(merged_data_ct_off_more_info$CategoryDisconnect[merged_data_ct_off_more_info$domain == domain]), collapse = ", "),
    CategoryLockdown = paste(unique(merged_data_ct_off_more_info$CategoryLockdown[merged_data_ct_off_more_info$domain == domain]), collapse = ", ")
  ) %>%
  ungroup()

print(tracker_summary_XL_extended_ct_off, n = 50)

# save as CSV
write.csv(tracker_summary_XL_extended_ct_off, "Output/Tables/most_hit_blacklist_trackers_XL_extended_ct_off.csv", row.names = TRUE)


## Create new df with only tracker entries
merged_data_ct_off_trackers_XL <- merged_data_ct_off_more_info %>% # (also for True Positives and False Negatives)
  filter(TrackerBlackListXL == TRUE) %>%
  select(domainOwner, DomainOwnerName, DomainOwnerNoTrack, DomainOwnerMaskedD, 
         AppName, domain, domainType, TrackerBlackList, TrackerBlackListXL, 
         CategoryNoTrack, CategoryDisconnect, OrganisationDisconnect, 
         EasyPrivacyTracker, stevenblackTracker, DisconnectTracker, MaskedDomainTracker, 
         FademindTracker, FrogeyeTracker, PrigentTracker, 
         LockdownPrivacyTracker, DanPollockTracker, AnudeepNDTracker,  EasyListTracker, YoyoTracker, 
         DeveloperdanTracker, NoTrackTracker, W3CTracker, FrogeyeMultiTracker, StevenblackXLTracker,
         firstTimeStamp, timeStamp, hits, bundleID, initiatedType, domainClassification)


# Show duplicate trackers (over multiple apps)
merged_data_ct_off_trackers_XL_duplicates <- merged_data_ct_off_trackers_XL %>%
  group_by(domain, AppName) %>%
  #, timeStamp, firstTimeStamp) %>%
  filter(n() > 1) %>%
  distinct(domain, AppName, .keep_all = TRUE)

# Show unique trackers
merged_data_ct_off_trackers_XL_unique <- merged_data_ct_off_trackers_XL %>%
  distinct(domain, .keep_all = TRUE)


### Save df with more Info as CSV ###
write.csv(merged_data_ct_off_more_info, "Output/Tables/merged_data_ct_off_more_info_df.csv", row.names = TRUE)


# 4.4 Filtering and narrowing down ---------------------------------------------

## Blacklist ---------------------------------------------------------------

#rm(merged_data_ct_off_trackers_domainType1)
#rm(merged_data_ct_off_trackers_domainType2)
#rm(merged_data_ct_off_blacklist_false_domainType2)
#rm(merged_data_ct_off_blacklist_false_domainType1)

merged_data_ct_off_trackers_domainType1 <- merged_data_ct_off_trackers %>%
  filter(domainType == 1) # for True Positives

merged_data_ct_off_trackers_domainType2 <- merged_data_ct_off_trackers %>%
  filter(domainType == 2) # for False Negatives

merged_data_ct_off_blacklist_false_domainType2 <- merged_data_ct_off_more_info %>%
  select(DomainOwnerName, AppName, domain, domainType, TrackerBlackList, #TrackerBlackListXL, 
         firstTimeStamp, timeStamp, hits, initiatedType, domainClassification) %>%
  filter(TrackerBlackList == FALSE & domainType == 2) # for True Negatives

merged_data_ct_off_blacklist_false_domainType1 <- merged_data_ct_off_more_info %>%
  select(DomainOwnerName, AppName, domain, domainType, TrackerBlackList, #TrackerBlackListXL, 
         firstTimeStamp, timeStamp, hits, initiatedType, domainClassification) %>%
  filter(TrackerBlackList == FALSE & domainType == 1) # for False Positives

#rm(confusion_matrix_ct_off)
## print table with number of True and False Positives and Negatives
confusion_matrix_ct_off <- table(merged_data_ct_off_more_info$domainType, merged_data_ct_off_more_info$TrackerBlackList)
confusion_matrix_ct_off <- addmargins(confusion_matrix_ct_off, FUN = list(Total = sum))
print(confusion_matrix_ct_off)

# write confusion matrix to CSV
write.csv(as.data.frame(confusion_matrix_ct_off), "Output/Tables/blacklist_domainType1_confusion_matrix_ct_off.csv", row.names = TRUE)


## Blacklist XL ------------------------------------------------------------

#rm(merged_data_ct_off_trackers_XL_domainType1)
#rm(merged_data_ct_off_trackers_XL_domainType2)
#rm(merged_data_ct_off_blacklist_XL_false_domainType2)
#rm(merged_data_ct_off_blacklist_XL_false_domainType1)

merged_data_ct_off_trackers_XL_domainType1 <- merged_data_ct_off_trackers_XL %>%
  filter(domainType == 1) # for True Positives

merged_data_ct_off_trackers_XL_domainType2 <- merged_data_ct_off_trackers_XL %>%
  filter(domainType == 2) # for False Negatives

merged_data_ct_off_blacklist_XL_false_domainType2 <- merged_data_ct_off_more_info %>%
  select(DomainOwnerName, AppName, domain, domainType, TrackerBlackList, TrackerBlackListXL, 
         firstTimeStamp, timeStamp, hits, initiatedType, domainClassification) %>%
  filter(TrackerBlackListXL == FALSE & domainType == 2) # for True Negatives

merged_data_ct_off_blacklist_XL_false_domainType1 <- merged_data_ct_off_more_info %>%
  select(DomainOwnerName, AppName, domain, domainType, TrackerBlackList, TrackerBlackListXL, 
         firstTimeStamp, timeStamp, hits, initiatedType, domainClassification) %>%
  filter(TrackerBlackListXL == FALSE & domainType == 1) # for False Positives


rm(confusion_matrix_XL_ct_off)
## print table with number of True and False Positives and Negatives
confusion_matrix_XL_ct_off <- table(merged_data_ct_off_more_info$domainType, merged_data_ct_off_more_info$TrackerBlackListXL)
confusion_matrix_XL_ct_off <- addmargins(confusion_matrix_XL_ct_off, FUN = list(Total = sum))
print(confusion_matrix_XL_ct_off)

# write confusion matrix to CSV
write.csv(as.data.frame(confusion_matrix_XL_ct_off), "Output/Tables/blacklist_domainType1_confusion_matrix_XL_ct_off.csv", row.names = TRUE)


## Opposite Df's -----------------------------------------------------------

#rm(merged_data_all_domainType1, merged_data_all_domainType2)

merged_data_ct_off_domainType1 <- merged_data_ct_off_more_info %>%
  select(DomainOwnerName, AppName, domain, domainType, TrackerBlackList, TrackerBlackListXL, 
         firstTimeStamp, timeStamp, hits, initiatedType, domainClassification) %>%
  filter(domainType == 1) # for True and False Positives

merged_data_ct_off_domainType2 <- merged_data_ct_off_more_info %>%
  select(DomainOwnerName, AppName, domain, domainType, TrackerBlackList, TrackerBlackListXL, 
         firstTimeStamp, timeStamp, hits, initiatedType, domainClassification) %>%
  filter(domainType == 2) # for True and False Negatives


# 5. CT ON - New Columns for domainOwner and Cross-Ref ----------------

# 5.1 DomainOwner --------------------------------------------------------------

# domain owner
merged_data_ct_on %>%
  group_by(domainOwner) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=76)

# number of unique domainOwner
length(unique(merged_data_ct_on$domainOwner))

# create new data frame with new column "DomainOwnerName" that renames all the domains that contain strings like "google", "apple", "facebook", "amazon", to that string
library(tidyverse) # For data manipulation


## Creating new column DomainOwnerName -----------------------------------------

merged_data_ct_on <- merged_data_ct_on %>%
  mutate(DomainOwnerName = case_when(
    str_detect(domainOwner, "Microsoft Corporation") ~ "Microsoft",
    str_detect(domainOwner, "Adobe Inc.", ) ~ "Adobe",
    str_detect(domainOwner, "AddApptr GmbH") ~ "AddApptr",
    str_detect(domainOwner, "Google LLC") ~ "Google",
    str_detect(domainOwner, "Facebook, Inc.") ~ "Meta",
    str_detect(domainOwner, "Unity Software Inc.") ~ "Unity Inc.",
    # additional DomainOwners
    str_detect(domainOwner, "Braze, Inc.") ~ "Braze Inc.",
    str_detect(domainOwner, "Urban Airship, Inc.") ~ "United Airship Inc.",
    str_detect(domainOwner, "SAP SE") ~ "SAP SE",
    str_detect(domainOwner, "Kochava") ~ "Kochava",
    str_detect(domainOwner, "AppLovin Corporation") ~ "AppLovin Corp.",
    str_detect(domainOwner, "RevenueCat") ~ "RevenueCat",
    str_detect(domainOwner, "Vungle Inc") ~ "Vungle Inc.",
    str_detect(domainOwner, "Amplitude") ~ "Amplitude",
    str_detect(domainOwner, "Digital Turbine") ~ "Digital Turbine",
    str_detect(domainOwner, "Snap Inc.") ~ "Snap Inc.",
    str_detect(domainOwner, "Amazon Technologies, Inc.") ~ "Amazon",
    str_detect(domainOwner, "AppsFlyer") ~ "AppsFlyer",
    str_detect(domainOwner, "Twitter, Inc.") ~ "Twitter Inc.",
    str_detect(domainOwner, "ByteDance Ltd.") ~ "ByteDance Ltd.",
    str_detect(domainOwner, "CleverTap") ~ "CleverTap",
    str_detect(domainOwner, "Moloco Inc.") ~ "Moloco Inc.",
    str_detect(domainOwner, "Criteo SA") ~ "Criteo SA",
    str_detect(domainOwner, "DataDome") ~ "DataDome",
    str_detect(domainOwner, "Forter Inc.") ~ "Forter Inc.",
    str_detect(domainOwner, "Bugsnag Inc.") ~ "Bugsnag Inc.",
    str_detect(domainOwner, "Datadog, Inc.") ~ "Datadog Inc.",
    str_detect(domainOwner, "InMobi Pte Ltd") ~ "InMobi Pte",
    str_detect(domainOwner, "Index Exchange, Inc.") ~ "Index Exchange",
    str_detect(domainOwner, "PayPal, Inc.") ~ "PayPal",
    str_detect(domainOwner, "Reddit Inc.") ~ "Reddit",
    str_detect(domainOwner, "OneSignal") ~ "OneSignal",
    str_detect(domainOwner, "OpenX Technologies Inc") ~ "OpenX Inc.",
    str_detect(domainOwner, "YieldMo, Inc.") ~ "YieldMo Inc.",
    str_detect(domainOwner, "Alibaba Group") ~ "Alibaba Group",
    str_detect(domainOwner, "IPONWEB GmbH") ~ "IPONWEB",
    str_detect(domainOwner, "Magnite, Inc.") ~ "Magnite Inc.",
    str_detect(domainOwner, "Mixpanel, Inc.") ~ "Mixpanel Inc.",
    str_detect(domainOwner, "The Trade Desk Inc") ~ "Trade Desk Inc.",
    str_detect(domainOwner, "TripleLift") ~ "TripleLift",
    str_detect(domainOwner, "Prospect One") ~ "Prospect One",
    str_detect(domainOwner, "Segment.io, Inc.") ~ "Segment Inc.",
    str_detect(domainOwner, "Vizbee, Inc.") ~ "Vizbee Inc.",
    str_detect(domainOwner, "comScore, Inc") ~ "comScore Inc.",
    str_detect(domainOwner, "Cloudflare, Inc.") ~ "Cloudflare",
    str_detect(domainOwner, "Iterable, Inc.") ~ "Iterable",
    str_detect(domainOwner, "LiveRamp") ~ "LiveRamp",
    str_detect(domainOwner, "Mobvista") ~ "Mobvista",
    str_detect(domainOwner, "Pulsepoint, Inc.") ~ "Pulsepoint",
    str_detect(domainOwner, "Salesforce.com, Inc.") ~ "Salesforce",
    str_detect(domainOwner, "ShareThis, Inc") ~ "ShareThis Inc.",
    str_detect(domainOwner, "Sift Science, Inc.") ~ "Sift Science Inc.",
    str_detect(domainOwner, "Verizon Media") ~ "Verizon Media",
    str_detect(domainOwner, "Zeta Global") ~ "Zeta Global",
    str_detect(domainOwner, "Fastly, Inc.") ~ "Fastly",
    str_detect(domainOwner, "GumGum") ~ "GumGum",
    str_detect(domainOwner, "Helpshift, Inc.") ~ "Helpshift",
    str_detect(domainOwner, "ID5 Technology Ltd") ~ "ID5 Technology",
    str_detect(domainOwner, "LiveIntent Inc.") ~ "LiveIntent Inc.",
    str_detect(domainOwner, "Lotame Solutions, Inc.") ~ "Lotame Solutions",
    str_detect(domainOwner, "Propeller Ads") ~ "Propeller Ads",
    str_detect(domainOwner, "PubMatic, Inc.") ~ "PubMatic Inc.",
    str_detect(domainOwner, "Quantcast Corporation") ~ "Quantcast Corp.",
    str_detect(domainOwner, "Sharethrough, Inc.") ~ "Sharethrough Inc.",
    str_detect(domainOwner, "Singular Labs, Inc.") ~ "Singular Labs",
    str_detect(domainOwner, "Sovrn Holdings") ~ "Sovrn Holdings",
    str_detect(domainOwner, "Teads \\( Luxenbourg \\) SA") ~ "Teads",
    str_detect(domainOwner, "The Rubicon Project, Inc.") ~ "Rubicon Project",
    str_detect(domainOwner, "Throtle") ~ "Throtle",
    str_detect(domainOwner, "Trustpilot A/S") ~ "Trustpilot",
    str_detect(domainOwner, "Undertone Networks") ~ "Undertone Networks",
    str_detect(domainOwner, "WarnerMedia, LLC") ~ "WarnerMedia",
    str_detect(domainOwner, "Wingify") ~ "Wingify",
    str_detect(domainOwner, "Yieldlove GmbH") ~ "Yieldlove",
    
    # adding via domains
    str_detect(domain, "google") ~ "Google",
    str_detect(domain, "youtube") ~ "Google", 
    str_detect(domain, "gstatic") ~ "Google",
    str_detect(domain, "googleapis") ~ "Google",
    str_detect(domain, "app-analytics-services.com") ~ "Google",
    str_detect(domain, "firebase-settings.crashlytics.com") ~ "Google",
    str_detect(domain, "firebaselogging-pa.googleapis.com") ~ "Google",
    str_detect(domain, "g.doubleclick") ~ "Google",
    str_detect(domain, "ad.doubleclick.net") ~ "Google",
    str_detect(domain, "apple") ~ "Apple",
    str_detect(domain, "mzstatic") ~ "Apple",
    str_detect(domain, "icloud") ~ "Apple",
    str_detect(domain, "itunes") ~ "Apple",
    str_detect(domain, "facebook") ~ "Meta",
    str_detect(domain, "whatsapp") ~ "Meta",
    str_detect(domain, "instagram") ~ "Meta",
    str_detect(domain, "threads") ~ "Meta",
    str_detect(domain, "fbcdn") ~ "Meta",
    str_detect(domain, "giphy") ~ "Shutterstock",
    #str_detect(domain, "messenger") ~ "Meta",
    str_detect(domain, "amazon") ~ "Amazon",
    str_detect(domain, "microsoft") ~ "Microsoft",
    str_detect(domain, "linkedin") ~ "Microsoft",
    str_detect(domain, "bing") ~ "Microsoft",
    str_detect(domain, "skype") ~ "Microsoft",
    str_detect(domain, "adobe") ~ "Adobe",
    str_detect(domain, "appsflyer") ~ "AppsFlyer",
    str_detect(domain, "unity3d") ~ "Unity Software",
    str_detect(domain, "unity.com") ~ "Unity Software",
    str_detect(domain, "ingest.sentry.io") ~ "Functional Software",
    str_detect(domain, "twitter") ~ "Twitter Inc.",
    str_detect(domain, "tiktok") ~ "ByteDance Ltd.",
    str_detect(domain, "mozilla") ~ "Mozilla Firefox",
    str_detect(domain, "spotify") ~ "Spotify",
    str_detect(domain, "reddit") ~ "Reddit",
    str_detect(domain, "snapchat") ~ "Snap Inc.",
    str_detect(domain, "tutti") ~ "SMG AG",
    str_detect(domain, "ricardo") ~ "SMG AG",
    str_detect(domain, "jodel") ~ "Jodel Venture GmbH",
    str_detect(domain, "strava") ~ "Strava Inc.",
    str_detect(domain, "signal.org") ~ "Signal Technology Foundation",
    str_detect(domain, "brave.com") ~ "Brave Software Inc.",
    str_detect(domain, "spiegel") ~ "Rudolf Augstein GmbH & Co. KG",
    str_detect(domain, "aliexpress") ~ "Alibaba Group",
    str_detect(domain, "alibaba") ~ "Alibaba Group",
    str_detect(domain, "alipay") ~ "Alibaba Group",
    str_detect(domain, "alicdn") ~ "Alibaba Group",
    str_detect(domain, "aliapp") ~ "Alibaba Group",
    str_detect(domain, "qq.com") ~ "Tencent Holdings Limited",
    str_detect(domain, "wechat.com") ~ "Tencent Holdings Limited",
    str_detect(domain, "tencent") ~ "Tencent Holdings Limited",
    str_detect(domain, "twint") ~ "TWINT AG",
    str_detect(domain, "20min") ~ "TX Group AG",
    str_detect(domain, "migro") ~ "Migros-Genossenschafts-Bund (MGB)",
    str_detect(domain, "srgssr") ~ "SRG SRF",
    str_detect(domain, "digitec") ~ "Digitec Galaxus AG",
    str_detect(domain, "galaxus") ~ "Digitec Galaxus AG",
    str_detect(domain, "sbb.ch") ~ "SBB CFF FFS",
    str_detect(domain, "adnxs") ~ "Microsoft",
    str_detect(domain, "doubleclick") ~ "Google",
    str_detect(domain, "bsky") ~ "Bluesky Social PBC",
    str_detect(domain, "doubleverify") ~ "DoubleVerify Holdings Inc.",
    str_detect(domain, "lencr") ~ "Let's Encrypt",
    str_detect(domain, "onesignal") ~ "OneSignal",
    str_detect(domain, "api3.branch.io") ~ "Branch",
    str_detect(domain, "app.usercentrics.eu") ~ "Usercentrics GmbH",
    str_detect(domain, "cacerts.digicert.com") ~ "DigiCert",
    str_detect(domain, "cdn.branch.io") ~ "Branch",
    str_detect(domain, "cdn.cookielaw.org") ~ "OneTrust",
    str_detect(domain, "census-app-x.scorecardresearch.com") ~ "comScore Inc.",
    str_detect(domain, "config.mapbox.com") ~ "Mapbox",
    str_detect(domain, "ocsp.digicert.com") ~ "DigiCert",
    str_detect(domain, "outlook.office365.com") ~ "Microsoft",
    str_detect(domain, "prod-mediate-events.applovin.com") ~ "AppLovin Corp.",
    str_detect(domain, "region1.app-analytics-services-att.com") ~ "AT&T Inc.",
    str_detect(domain, "static.zdassets.com") ~ "Zendesk",
    
    
    TRUE ~ "Other"
  ))

merged_data_ct_on <- merged_data_ct_on %>% 
  select(firstTimeStamp, timeStamp, hits, bundleID, AppName, domain, domainOwner, DomainOwnerName, everything()) 

# Save df as CSV
write.csv(merged_data_ct_on, "Output/Tables/merged_data_ct_on_df.csv", row.names = TRUE)


## Printing DomainOwnerName summaries ------------------------------------------

library(dplyr)
merged_data_ct_on %>%
  group_by(DomainOwnerName) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=104) %>%
  # add new empty "notes" column
  mutate(notes = case_when(
    TRUE ~ "" # Default case for all other domains
  )) %>%
  write.csv("Output/Tables/DomainOwnerName_summary_ct_on.csv", row.names = TRUE)


merged_data_ct_on %>%
  group_by(domain) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=150) 


## Filtered Df domain >= 5 -----------------------------------------------------

# New df with domains that have more than 5 accesses
merged_data_ct_on_domain_10plus <- merged_data_ct_on %>%
  group_by(domain) %>%
  summarise(total_accesses = n()) %>%
  filter(total_accesses >= 5) %>%
  arrange(desc(total_accesses)) %>%
  # add DomainOwnerName and domainType columns
  left_join(merged_data_ct_on %>% 
              select(domain, domainOwner, DomainOwnerName, domainType, domainClassification, initiatedType) %>%
              distinct(), by = "domain") %>%
  # add new empty "notes" column
  mutate(notes = case_when(
    TRUE ~ "" # Default case for all other domains
  )) %>%
  print(n=150)

# Save df as CSV
write.csv(merged_data_ct_on_domain_10plus, "Output/Tables/most_accessed_domains_10plus_ct_on.csv", row.names = TRUE)


# New df with domains that have more than 10 accesses and all selected columns
merged_data_ct_on_domain_10plus_full <- merged_data_ct_on %>%
  filter(domain %in% merged_data_ct_on_domain_10plus$domain) %>%
  select(firstTimeStamp, timeStamp, hits, bundleID, AppName, domain, domainOwner, DomainOwnerName, everything()) %>%
  arrange(desc(timeStamp)) 

# Save df as CSV
write.csv(merged_data_ct_on_domain_10plus_full, "Output/Tables/most_accessed_domains_10plus_full_ct_on.csv", row.names = TRUE)


## New df's with domain Owners -------------------------------------------------

# New df for domain Owner Names with domains that have more than 10 accesses
merged_data_ct_on_domain_10plus_domainOwners <- merged_data_ct_on_domain_10plus %>%
  #group_by(domainOwner) %>%
  group_by(DomainOwnerName) %>%
  summarise(total_accesses = n()) %>%
  #filter(total_accesses > 10) %>%
  arrange(desc(total_accesses)) %>%
  mutate(notes = case_when(   # add new empty "notes" column
    TRUE ~ "" # Default case for all other domains
  )) %>%
  print(n=150)

write.csv(merged_data_ct_on_domain_10plus_domainOwners, "Output/Tables/DomainOwnerName_summary_10plus_ct_on.csv", row.names = TRUE)

# Count domainType
table(merged_data_ct_on$domainType)
table(merged_data_ct_on_domain_10plus$domainType)

5940/100
629/59.4

# unique number of domains where domainType = 1
length(unique(merged_data_ct_on$domain[merged_data_ct_on$domainType == 1])) # 177

length(unique(merged_data_ct_on$domain[merged_data_ct_on$domainType == 2])) # 2807

# unique number of bundleIDs
length(unique(merged_data_ct_on$bundleID)) # 138


# 5.2 Adding tracker info to merged_data_ct_on_more_info -----------------------

library(dplyr)

# remove leading www. from domain in merged_data_ct_on
merged_data_ct_on <- merged_data_ct_on %>%
  mutate(domain = str_remove(domain, "^www\\."))

#rm(merged_data_ct_on_more_info)
merged_data_ct_on_more_info <- merged_data_ct_on

## Disconnect ----------------------------------------------------

merged_data_ct_on_more_info <- merged_data_ct_on_more_info %>%
  mutate(DisconnectTracker = domain %in% disconnect_df$domain)

merged_data_ct_on_more_info <- merged_data_ct_on_more_info %>%
  left_join(
    disconnect_df %>% 
      select(domain, OrganisationDisconnect, CategoryDisconnect), by = "domain") #%>%
#distinct()

#count number of TRUE in disconnect_tracker
table(merged_data_ct_on_more_info$DisconnectTracker)
table(merged_data_ct_on_more_info$domainType)
table(merged_data_ct_on_more_info$domainType, merged_data_ct_on_more_info$DisconnectTracker)


## EasyPrivacy -----------------------------

library(dplyr)
# find all network activity entries that match EasyList EasyPrivacy and add a tracker flag 
merged_data_ct_on_more_info <- merged_data_ct_on_more_info %>%
  mutate(EasyPrivacyTracker = domain %in% easyprivacy_df$domain)

#count number of TRUE in easylist_tracker
table(merged_data_ct_on_more_info$EasyPrivacyTracker)
table(merged_data_ct_on_more_info$domainType)
table(merged_data_ct_on_more_info$domainType, merged_data_ct_on_more_info$EasyPrivacyTracker)


## Summary
#tracker_summary_easylist <- merged_data_ct_on_more_info %>%
#filter(EasyPrivacyTracker) %>%
#group_by(domain) %>%
#summarise(total_hits = n(), .groups = "drop") %>%
#arrange(desc(total_hits))

#print(tracker_summary_easylist, n = 50)


## Stevenblack ---------------------------------------------------

merged_data_ct_on_more_info <- merged_data_ct_on_more_info %>%
  mutate(stevenblackTracker = domain %in% stevenblack_df$domain)

#count number of TRUE in easylist_tracker
table(merged_data_ct_on_more_info$stevenblackTracker)
table(merged_data_ct_on_more_info$domainType)
table(merged_data_ct_on_more_info$domainType, merged_data_ct_on_more_info$stevenblackTracker)


## Masked Domain -------------------------------------------------

merged_data_ct_on_more_info <- merged_data_ct_on_more_info %>%
  mutate(MaskedDomainTracker = domain %in% masked_domain_df$domain) %>%
  # add DomainOwnerMaskedD info column
  left_join(masked_domain_df %>% 
              select(domain, DomainOwnerMaskedD), by = "domain") #%>%
#distinct()

#count number of TRUE in masked_domain_tracker
table(merged_data_ct_on_more_info$MaskedDomainTracker)
table(merged_data_ct_on_more_info$domainType)
table(merged_data_ct_on_more_info$domainType, merged_data_ct_on_more_info$MaskedDomainTracker)


## NoTrack -------------------------------------------------------

merged_data_ct_on_more_info <- merged_data_ct_on_more_info %>%
  mutate(NoTrackTracker = domain %in% notrack_df$domain)

merged_data_ct_on_more_info <- merged_data_ct_on_more_info %>%
  left_join(
    notrack_df %>% 
      select(domain, DomainOwnerNoTrack, CategoryNoTrack), by = "domain") #%>%
#distinct()

#count number of TRUE in notrack_tracker
table(merged_data_ct_on_more_info$NoTrackTracker)
table(merged_data_ct_on_more_info$domainType)
table(merged_data_ct_on_more_info$domainType, merged_data_ct_on_more_info$NoTrackTracker)


## Prigent -------------------------------------------------------

merged_data_ct_on_more_info <- merged_data_ct_on_more_info %>%
  mutate(PrigentTracker = domain %in% prigent_df$domain)

#count number of TRUE in prigent_tracker
table(merged_data_ct_on_more_info$PrigentTracker)
table(merged_data_ct_on_more_info$domainType)
table(merged_data_ct_on_more_info$domainType, merged_data_ct_on_more_info$PrigentTracker)


## Fademind -------------------------------------------------------

merged_data_ct_on_more_info <- merged_data_ct_on_more_info %>%
  mutate(FademindTracker = domain %in% fademind_df$domain)

#count number of TRUE in fademind_tracker
table(merged_data_ct_on_more_info$FademindTracker)
table(merged_data_ct_on_more_info$domainType)
table(merged_data_ct_on_more_info$domainType, merged_data_ct_on_more_info$FademindTracker)


## Frogeye -------------------------------------------------------

merged_data_ct_on_more_info <- merged_data_ct_on_more_info %>%
  mutate(FrogeyeTracker = domain %in% frogeye_df$domain)

#count number of TRUE in frogeye_tracker
table(merged_data_ct_on_more_info$FrogeyeTracker)
table(merged_data_ct_on_more_info$domainType)
table(merged_data_ct_on_more_info$domainType, merged_data_ct_on_more_info$FrogeyeTracker)


## lockdown privacy --------------------------------------------------------

merged_data_ct_on_more_info <- merged_data_ct_on_more_info %>%
  mutate(LockdownPrivacyTracker = domain %in% lockdown_privacy_df$domain) %>%
  left_join(
    lockdown_privacy_df %>% 
      select(domain, CategoryLockdown), by = "domain") #%>%
#distinct()

#count number of TRUE in lockdownprivacy_tracker
table(merged_data_ct_on_more_info$LockdownPrivacyTracker)
table(merged_data_ct_on_more_info$domainType)
table(merged_data_ct_on_more_info$domainType, merged_data_ct_on_more_info$LockdownPrivacyTracker)      


## Dan Pollock -------------------------------------------------------

merged_data_ct_on_more_info <- merged_data_ct_on_more_info %>%
  mutate(DanPollockTracker = domain %in% dan_pollock_df$domain)

#count number of TRUE in danpollock_tracker
table(merged_data_ct_on_more_info$DanPollockTracker)
table(merged_data_ct_on_more_info$domainType)
table(merged_data_ct_on_more_info$domainType, merged_data_ct_on_more_info$DanPollockTracker)


## AnudeepND -------------------------------------------------------

merged_data_ct_on_more_info <- merged_data_ct_on_more_info %>%
  mutate(AnudeepNDTracker = domain %in% anudeepND_df$domain)

#count number of TRUE in anudeepND_tracker
table(merged_data_ct_on_more_info$AnudeepNDTracker)
table(merged_data_ct_on_more_info$domainType)
table(merged_data_ct_on_more_info$domainType, merged_data_ct_on_more_info$AnudeepNDTracker)


## EasyList -------------------------------------------------------

merged_data_ct_on_more_info <- merged_data_ct_on_more_info %>%
  mutate(EasyListTracker = domain %in% easylist_df$domain)

#count number of TRUE in easylist_tracker
table(merged_data_ct_on_more_info$EasyListTracker)
table(merged_data_ct_on_more_info$domainType)
table(merged_data_ct_on_more_info$domainType, merged_data_ct_on_more_info$EasyListTracker)


## Developerdan -------------------------------------------------------

merged_data_ct_on_more_info <- merged_data_ct_on_more_info %>%
  mutate(DeveloperdanTracker = domain %in% developerdan_df$domain)

#count number of TRUE in developerdan_tracker
table(merged_data_ct_on_more_info$DeveloperdanTracker)
table(merged_data_ct_on_more_info$domainType)
table(merged_data_ct_on_more_info$domainType, merged_data_ct_on_more_info$DeveloperdanTracker)


## Yoyo -------------------------------------------------------

merged_data_ct_on_more_info <- merged_data_ct_on_more_info %>%
  mutate(YoyoTracker = domain %in% yoyo_df$domain)

#count number of TRUE in yoyo_tracker
table(merged_data_ct_on_more_info$YoyoTracker)
table(merged_data_ct_on_more_info$domainType)
table(merged_data_ct_on_more_info$domainType, merged_data_ct_on_more_info$YoyoTracker)


## W3C -------------------------------------------------------

merged_data_ct_on_more_info <- merged_data_ct_on_more_info %>%
  mutate(W3CTracker = domain %in% w3c_df$domain)

#count number of TRUE in w3c_tracker
table(merged_data_ct_on_more_info$W3CTracker)
table(merged_data_ct_on_more_info$domainType)
table(merged_data_ct_on_more_info$domainType, merged_data_ct_on_more_info$W3CTracker)


## Frogeye Multi -------------------------------------------------------

merged_data_ct_on_more_info <- merged_data_ct_on_more_info %>%
  mutate(FrogeyeMultiTracker = domain %in% frogeye_multi_df$domain)

#count number of TRUE in frogeye_multi_tracker
table(merged_data_ct_on_more_info$FrogeyeMultiTracker)
table(merged_data_ct_on_more_info$domainType)
table(merged_data_ct_on_more_info$domainType, merged_data_ct_on_more_info$FrogeyeMultiTracker)


## Stevenblack XL --------------------------------------------------------------

merged_data_ct_on_more_info <- merged_data_ct_on_more_info %>%
  mutate(StevenblackXLTracker = domain %in% stevenblack_df_XL$domain)

#count number of TRUE in stevenblack_XL_tracker
table(merged_data_ct_on_more_info$StevenblackXLTracker)
table(merged_data_ct_on_more_info$domainType)
table(merged_data_ct_on_more_info$domainType, merged_data_ct_on_more_info$StevenblackXLTracker)


# 5.3 Cross-reference all with network activity data ---------------------------

## Blacklist -------------------------------------------------------------------

## merge with blacklist df
merged_data_ct_on_more_info <- merged_data_ct_on_more_info %>%
  mutate(TrackerBlackList = domain %in% blacklist_df$domain)

#count number of TRUE in TrackerBlackList
table(merged_data_ct_on_more_info$TrackerBlackList)
table(merged_data_ct_on_more_info$domainType)
table(merged_data_ct_on_more_info$domainType, merged_data_ct_on_more_info$TrackerBlackList)


## Summary - with True and False Positives and False Negatives
tracker_summary_ct_on <- merged_data_ct_on_more_info %>%
  filter(TrackerBlackList == TRUE | domainType == 1) %>% # == TRUE or domainType == 1
  #filter(TrackerBlackList) %>%
  group_by(domain) %>%
  summarise(total_hits = n(), .groups = "drop") %>%
  arrange(desc(total_hits))

print(tracker_summary_ct_on, n = 50)

# save as CSV
write.csv(tracker_summary_ct_on, "Output/Tables/most_hit_blacklist_trackers_ct_on.csv", row.names = TRUE)


## Summary with more info
#rm(tracker_summary)
tracker_summary_extended_ct_on <- merged_data_ct_on_more_info %>%
  filter(TrackerBlackList == TRUE | domainType == 1) %>% # == TRUE or domainType == 1
  group_by(domain) %>%
  summarise(total_hits = n(), .groups = "drop") %>%
  arrange(desc(total_hits)) %>%
  left_join(
    merged_data_ct_on_more_info %>%
      select(domain, TrackerBlackList) %>%
      distinct(), by = "domain"
  ) %>%
  # add columns with per domain occurring domainTypes and apps and initiatedType
  rowwise() %>%
  mutate(
    domainType = paste(unique(merged_data_ct_on_more_info$domainType[merged_data_ct_on_more_info$domain == domain]), collapse = ", "),
    domainClassification = paste(unique(merged_data_ct_on_more_info$domainClassification[merged_data_ct_on_more_info$domain == domain]), collapse = ", "),
    hits_sum = sum(merged_data_ct_on_more_info$hits[merged_data_ct_on_more_info$domain == domain]),
    #hits = paste(merged_data_ct_on_more_info$hits[merged_data_ct_on_more_info$domain == domain], collapse = ", "),
    initiatedType = paste(unique(merged_data_ct_on_more_info$initiatedType[merged_data_ct_on_more_info$domain == domain]), collapse = ", "),
    DomainOwnerName = paste(unique(merged_data_ct_on_more_info$DomainOwnerName[merged_data_ct_on_more_info$domain == domain]), collapse = ", "),
    apps = paste(unique(merged_data_ct_on_more_info$AppName[merged_data_ct_on_more_info$domain == domain]), collapse = ", "),
    apps_count = length(unique(merged_data_ct_on_more_info$AppName[merged_data_ct_on_more_info$domain == domain])),
    CategoryNoTrack = paste(unique(merged_data_ct_on_more_info$CategoryNoTrack[merged_data_ct_on_more_info$domain == domain]), collapse = ", "),
    CategoryDisconnect = paste(unique(merged_data_ct_on_more_info$CategoryDisconnect[merged_data_ct_on_more_info$domain == domain]), collapse = ", "),
    CategoryLockdown = paste(unique(merged_data_ct_on_more_info$CategoryLockdown[merged_data_ct_on_more_info$domain == domain]), collapse = ", ")
  ) %>%
  ungroup()

print(tracker_summary_extended_ct_on, n = 50)

# save as CSV
write.csv(tracker_summary_extended_ct_on, "Output/Tables/most_hit_blacklist_trackers_extended_ct_on.csv", row.names = TRUE)



## Create new df with only tracker entries
#rm(merged_data_all_trackers)
merged_data_ct_on_trackers <- merged_data_ct_on_more_info %>% # (also for True Positives and False Negatives)
  filter(TrackerBlackList == TRUE) %>%
  select(domainOwner, DomainOwnerName, DomainOwnerNoTrack, DomainOwnerMaskedD, 
         AppName, domain, domainType, TrackerBlackList, #TrackerBlackListXL, 
         CategoryNoTrack, CategoryDisconnect, OrganisationDisconnect, 
         EasyPrivacyTracker, stevenblackTracker, DisconnectTracker, MaskedDomainTracker, 
         FademindTracker, FrogeyeTracker, PrigentTracker, 
         LockdownPrivacyTracker, DanPollockTracker, AnudeepNDTracker,  EasyListTracker, YoyoTracker, 
         #DeveloperdanTracker, NoTrackTracker, W3CTracker, FrogeyeMultiTracker, StevenblackXLTracker,
         firstTimeStamp, timeStamp, hits, bundleID, initiatedType, domainClassification)


# Show duplicate trackers (over multiple apps)
#rm(merged_data_all_trackers_duplicates)
merged_data_ct_on_trackers_duplicates <- merged_data_ct_on_trackers %>%
  group_by(domain, AppName) %>%
  #, timeStamp, firstTimeStamp) %>%
  filter(n() > 1) %>%
  distinct(domain, AppName, .keep_all = TRUE)

# Show unique trackers
#rm(merged_data_all_trackers_unique)
merged_data_ct_on_trackers_unique <- merged_data_ct_on_trackers %>%
  distinct(domain, .keep_all = TRUE)


## Blacklist XL ----------------------------------------------------------------

## merge with blacklist df XL
merged_data_ct_on_more_info <- merged_data_ct_on_more_info %>%
  mutate(TrackerBlackListXL = domain %in% blacklist_XL_df$domain)

#count number of TRUE in TrackerBlackListXL
table(merged_data_ct_on_more_info$TrackerBlackListXL)
table(merged_data_ct_on_more_info$domainType)
table(merged_data_ct_on_more_info$domainType, merged_data_ct_on_more_info$TrackerBlackListXL)

# Summary XL - with True and False Positives and False Negatives
tracker_summary_XL_ct_on <- merged_data_ct_on_more_info %>%
  filter(TrackerBlackListXL == TRUE | domainType == 1) %>% # == TRUE or domainType == 1
  #filter(TrackerBlackListXL) %>%
  group_by(domain) %>%
  summarise(total_hits = n(), .groups = "drop") %>%
  arrange(desc(total_hits))

print(tracker_summary_XL_ct_on, n = 50)

# save as CSV
write.csv(tracker_summary_XL_ct_on, "Output/Tables/most_hit_blacklist_XL_trackers_ct_on.csv", row.names = TRUE)


## Summary with more info
#rm(tracker_summary_extended_XL_ct_on)
tracker_summary_XL_extended_ct_on <- merged_data_ct_on_more_info %>%
  filter(TrackerBlackListXL == TRUE | domainType == 1) %>% # == TRUE or domainType == 1
  group_by(domain) %>%
  summarise(total_hits = n(), .groups = "drop") %>%
  arrange(desc(total_hits)) %>%
  left_join(
    merged_data_ct_on_more_info %>%
      select(domain, TrackerBlackListXL, TrackerBlackList) %>%
      distinct(), by = "domain"
  ) %>%
  # add columns with per domain occurring domainTypes and apps and initiatedType
  rowwise() %>%
  mutate(
    domainType = paste(unique(merged_data_ct_on_more_info$domainType[merged_data_ct_on_more_info$domain == domain]), collapse = ", "),
    domainClassification = paste(unique(merged_data_ct_on_more_info$domainClassification[merged_data_ct_on_more_info$domain == domain]), collapse = ", "),
    hits_sum = sum(merged_data_ct_on_more_info$hits[merged_data_ct_on_more_info$domain == domain]),
    #hits = paste(merged_data_ct_on_more_info$hits[merged_data_ct_on_more_info$domain == domain], collapse = ", "),
    initiatedType = paste(unique(merged_data_ct_on_more_info$initiatedType[merged_data_ct_on_more_info$domain == domain]), collapse = ", "),
    DomainOwnerName = paste(unique(merged_data_ct_on_more_info$DomainOwnerName[merged_data_ct_on_more_info$domain == domain]), collapse = ", "),
    apps = paste(unique(merged_data_ct_on_more_info$AppName[merged_data_ct_on_more_info$domain == domain]), collapse = ", "),
    apps_count = length(unique(merged_data_ct_on_more_info$AppName[merged_data_ct_on_more_info$domain == domain])),
    CategoryNoTrack = paste(unique(merged_data_ct_on_more_info$CategoryNoTrack[merged_data_ct_on_more_info$domain == domain]), collapse = ", "),
    CategoryDisconnect = paste(unique(merged_data_ct_on_more_info$CategoryDisconnect[merged_data_ct_on_more_info$domain == domain]), collapse = ", "),
    CategoryLockdown = paste(unique(merged_data_ct_on_more_info$CategoryLockdown[merged_data_ct_on_more_info$domain == domain]), collapse = ", ")
  ) %>%
  ungroup()

print(tracker_summary_XL_extended_ct_on, n = 50)

# save as CSV
write.csv(tracker_summary_XL_extended_ct_on, "Output/Tables/most_hit_blacklist_XL_trackers_extended_ct_on.csv", row.names = TRUE)


## Create new df with only tracker entries
merged_data_ct_on_trackers_XL <- merged_data_ct_on_more_info %>% # (also for True Positives and False Negatives)
  filter(TrackerBlackListXL == TRUE) %>%
  select(domainOwner, DomainOwnerName, DomainOwnerNoTrack, DomainOwnerMaskedD, 
         AppName, domain, domainType, TrackerBlackList, TrackerBlackListXL, 
         CategoryNoTrack, CategoryDisconnect, OrganisationDisconnect, 
         EasyPrivacyTracker, stevenblackTracker, DisconnectTracker, MaskedDomainTracker, 
         FademindTracker, FrogeyeTracker, PrigentTracker, 
         LockdownPrivacyTracker, DanPollockTracker, AnudeepNDTracker,  EasyListTracker, YoyoTracker, 
         DeveloperdanTracker, NoTrackTracker, W3CTracker, FrogeyeMultiTracker, StevenblackXLTracker,
         firstTimeStamp, timeStamp, hits, bundleID, initiatedType, domainClassification)


# Show duplicate trackers (over multiple apps)
merged_data_ct_on_trackers_XL_duplicates <- merged_data_ct_on_trackers_XL %>%
  group_by(domain, AppName) %>%
  #, timeStamp, firstTimeStamp) %>%
  filter(n() > 1) %>%
  distinct(domain, AppName, .keep_all = TRUE)

# Show unique trackers
merged_data_ct_on_trackers_XL_unique <- merged_data_ct_on_trackers_XL %>%
  distinct(domain, .keep_all = TRUE)


### Save df with more Info as CSV ###
write.csv(merged_data_ct_on_more_info, "Output/Tables/merged_data_ct_on_more_info_df.csv", row.names = TRUE)


# 5.4 Filtering and narrowing down ---------------------------------------------

## Blacklist -------------------------------------------------------------------

#rm(merged_data_ct_on_trackers_domainType1)
#rm(merged_data_ct_on_trackers_domainType2)

merged_data_ct_on_trackers_domainType1 <- merged_data_ct_on_trackers %>%
  filter(domainType == 1) # for True Positives

merged_data_ct_on_trackers_domainType2 <- merged_data_ct_on_trackers %>%
  filter(domainType == 2) # for False Negatives

merged_data_ct_on_blacklist_false_domainType2 <- merged_data_ct_on_more_info %>%
  select(DomainOwnerName, AppName, domain, domainType, TrackerBlackList, #TrackerBlackListXL, 
         firstTimeStamp, timeStamp, hits, initiatedType, domainClassification) %>%
  filter(TrackerBlackList == FALSE & domainType == 2) # for True Negatives

merged_data_ct_on_blacklist_false_domainType1 <- merged_data_ct_on_more_info %>%
  select(DomainOwnerName, AppName, domain, domainType, TrackerBlackList, #TrackerBlackListXL, 
         firstTimeStamp, timeStamp, hits, initiatedType, domainClassification) %>%
  filter(TrackerBlackList == FALSE & domainType == 1) # for False Positives


## print table with number of True and False Positives and Negatives
confusion_matrix_ct_on <- table(merged_data_ct_on_more_info$domainType, merged_data_ct_on_more_info$TrackerBlackList)
confusion_matrix_ct_on <- addmargins(confusion_matrix_ct_on, FUN = list(Total = sum))
print(confusion_matrix_ct_on)

# write confusion matrix to CSV
write.csv(as.data.frame(confusion_matrix_ct_on), "Output/Tables/blacklist_domainType1_confusion_matrix_ct_on.csv", row.names = TRUE)


## Blacklist XL ----------------------------------------------------------------

#rm(merged_data_ct_on_trackers_XL_domainType1)
#rm(merged_data_ct_on_trackers_XL_domainType2)
#rm(merged_data_ct_on_blacklist_XL_false_domainType2)
#rm(merged_data_ct_on_blacklist_XL_false_domainType1)

merged_data_ct_on_trackers_XL_domainType1 <- merged_data_ct_on_trackers_XL %>%
  filter(domainType == 1) # for True Positives

merged_data_ct_on_trackers_XL_domainType2 <- merged_data_ct_on_trackers_XL %>%
  filter(domainType == 2) # for False Negatives

merged_data_ct_on_blacklist_XL_false_domainType2 <- merged_data_ct_on_more_info %>%
  select(DomainOwnerName, AppName, domain, domainType, TrackerBlackList, TrackerBlackListXL, 
         firstTimeStamp, timeStamp, hits, initiatedType, domainClassification) %>%
  filter(TrackerBlackListXL == FALSE & domainType == 2) # for True Negatives

merged_data_ct_on_blacklist_XL_false_domainType1 <- merged_data_ct_on_more_info %>%
  select(DomainOwnerName, AppName, domain, domainType, TrackerBlackList, TrackerBlackListXL, 
         firstTimeStamp, timeStamp, hits, initiatedType, domainClassification) %>%
  filter(TrackerBlackListXL == FALSE & domainType == 1) # for False Positives


## print table with number of True and False Positives and Negatives
confusion_matrix_XL_ct_on <- table(merged_data_ct_on_more_info$domainType, merged_data_ct_on_more_info$TrackerBlackListXL)
confusion_matrix_XL_ct_on <- addmargins(confusion_matrix_XL_ct_on, FUN = list(Total = sum))
print(confusion_matrix_XL_ct_on)

# write confusion matrix to CSV
write.csv(as.data.frame(confusion_matrix_XL_ct_on), "Output/Tables/blacklist_XL_domainType1_confusion_matrix_ct_on.csv", row.names = TRUE)


## Opposite Df's ---------------------------------------------------------------

#rm(merged_data_all_domainType1, merged_data_all_domainType2)

merged_data_ct_on_domainType1 <- merged_data_ct_on_more_info %>%
  select(DomainOwnerName, AppName, domain, domainType, TrackerBlackList, TrackerBlackListXL, 
         firstTimeStamp, timeStamp, hits, initiatedType, domainClassification) %>%
  filter(domainType == 1) # for True and False Positives

merged_data_ct_on_domainType2 <- merged_data_ct_on_more_info %>%
  select(DomainOwnerName, AppName, domain, domainType, TrackerBlackList, TrackerBlackListXL, 
         firstTimeStamp, timeStamp, hits, initiatedType, domainClassification) %>%
  filter(domainType == 2) # for True and False Negatives


# 6. Comparison CT ON & OFF --------------------------------------------------

# compare number of suspected trackers (True & False Positives + False Negatives) in CT ON vs CT OFF
nrow(tracker_summary_ct_off)
nrow(tracker_summary_ct_on)

nrow(tracker_summary_XL_ct_off)
nrow(tracker_summary_XL_ct_on)

# print confusion matrices
print(confusion_matrix)
print(confusion_matrix_XL)

print(confusion_matrix_ct_off)
print(confusion_matrix_ct_on)

print(confusion_matrix_XL_ct_off)
print(confusion_matrix_XL_ct_on)

# calculate ratios TRUE + FALSE POSITIVES / TOTAL ENTRIES
confusion_matrix_ct_off[1,3] / confusion_matrix_ct_off[3,3]
confusion_matrix_ct_on[1,3] / confusion_matrix_ct_on[3,3]


## Confusion Matrix Ratios -------------------------------------------------

confusion_matrix_ratio <- confusion_matrix
confusion_matrix_ratio[1,1] <- confusion_matrix[1,1] / confusion_matrix[3,3] * 100
confusion_matrix_ratio[1,2] <- confusion_matrix[1,2] / confusion_matrix[3,3] * 100
confusion_matrix_ratio[2,1] <- confusion_matrix[2,1] / confusion_matrix[3,3] * 100
confusion_matrix_ratio[2,2] <- confusion_matrix[2,2] / confusion_matrix[3,3] * 100
confusion_matrix_ratio[1,3] <- confusion_matrix[1,3] / confusion_matrix[3,3] * 100 
confusion_matrix_ratio[2,3] <- confusion_matrix[2,3] / confusion_matrix[3,3] * 100
confusion_matrix_ratio[3,1] <- confusion_matrix[3,1] / confusion_matrix[3,3] * 100
confusion_matrix_ratio[3,2] <- confusion_matrix[3,2] / confusion_matrix[3,3] * 100
confusion_matrix_ratio[3,3] <- confusion_matrix[3,3] / confusion_matrix[3,3] * 100
print(confusion_matrix_ratio)

confusion_matrix_XL_ratio <- confusion_matrix_XL
confusion_matrix_XL_ratio[1,1] <- confusion_matrix_XL[1,1] / confusion_matrix_XL[3,3] * 100
confusion_matrix_XL_ratio[1,2] <- confusion_matrix_XL[1,2] / confusion_matrix_XL[3,3] * 100
confusion_matrix_XL_ratio[2,1] <- confusion_matrix_XL[2,1] / confusion_matrix_XL[3,3] * 100
confusion_matrix_XL_ratio[2,2] <- confusion_matrix_XL[2,2] / confusion_matrix_XL[3,3] * 100
confusion_matrix_XL_ratio[1,3] <- confusion_matrix_XL[1,3] / confusion_matrix_XL[3,3] * 100 
confusion_matrix_XL_ratio[2,3] <- confusion_matrix_XL[2,3] / confusion_matrix_XL[3,3] * 100
confusion_matrix_XL_ratio[3,1] <- confusion_matrix_XL[3,1] / confusion_matrix_XL[3,3] * 100
confusion_matrix_XL_ratio[3,2] <- confusion_matrix_XL[3,2] / confusion_matrix_XL[3,3] * 100
confusion_matrix_XL_ratio[3,3] <- confusion_matrix_XL[3,3] / confusion_matrix_XL[3,3] * 100
print(confusion_matrix_XL_ratio)

# calculate ratios of all entries in confusion matrix in new confusion matrix
confusion_matrix_ct_off_ratio <- confusion_matrix_ct_off
confusion_matrix_ct_off_ratio[1,1] <- confusion_matrix_ct_off[1,1] / confusion_matrix_ct_off[3,3] * 100
confusion_matrix_ct_off_ratio[1,2] <- confusion_matrix_ct_off[1,2] / confusion_matrix_ct_off[3,3] * 100
confusion_matrix_ct_off_ratio[2,1] <- confusion_matrix_ct_off[2,1] / confusion_matrix_ct_off[3,3] * 100
confusion_matrix_ct_off_ratio[2,2] <- confusion_matrix_ct_off[2,2] / confusion_matrix_ct_off[3,3] * 100
confusion_matrix_ct_off_ratio[1,3] <- confusion_matrix_ct_off[1,3] / confusion_matrix_ct_off[3,3] * 100 
confusion_matrix_ct_off_ratio[2,3] <- confusion_matrix_ct_off[2,3] / confusion_matrix_ct_off[3,3] * 100
confusion_matrix_ct_off_ratio[3,1] <- confusion_matrix_ct_off[3,1] / confusion_matrix_ct_off[3,3] * 100
confusion_matrix_ct_off_ratio[3,2] <- confusion_matrix_ct_off[3,2] / confusion_matrix_ct_off[3,3] * 100
confusion_matrix_ct_off_ratio[3,3] <- confusion_matrix_ct_off[3,3] / confusion_matrix_ct_off[3,3] * 100
print(confusion_matrix_ct_off_ratio)

confusion_matrix_ct_on_ratio <- confusion_matrix_ct_on
confusion_matrix_ct_on_ratio[1,1] <- confusion_matrix_ct_on[1,1] / confusion_matrix_ct_on[3,3] * 100
confusion_matrix_ct_on_ratio[1,2] <- confusion_matrix_ct_on[1,2] / confusion_matrix_ct_on[3,3] * 100
confusion_matrix_ct_on_ratio[2,1] <- confusion_matrix_ct_on[2,1] / confusion_matrix_ct_on[3,3] * 100
confusion_matrix_ct_on_ratio[2,2] <- confusion_matrix_ct_on[2,2] / confusion_matrix_ct_on[3,3] * 100
confusion_matrix_ct_on_ratio[1,3] <- confusion_matrix_ct_on[1,3] / confusion_matrix_ct_on[3,3] * 100 
confusion_matrix_ct_on_ratio[2,3] <- confusion_matrix_ct_on[2,3] / confusion_matrix_ct_on[3,3] * 100
confusion_matrix_ct_on_ratio[3,1] <- confusion_matrix_ct_on[3,1] / confusion_matrix_ct_on[3,3] * 100
confusion_matrix_ct_on_ratio[3,2] <- confusion_matrix_ct_on[3,2] / confusion_matrix_ct_on[3,3] * 100
confusion_matrix_ct_on_ratio[3,3] <- confusion_matrix_ct_on[3,3] / confusion_matrix_ct_on[3,3] * 100
print(confusion_matrix_ct_on_ratio)

confusion_matrix_XL_ct_off_ratio <- confusion_matrix_XL_ct_off
confusion_matrix_XL_ct_off_ratio[1,1] <- confusion_matrix_XL_ct_off[1,1] / confusion_matrix_XL_ct_off[3,3] * 100
confusion_matrix_XL_ct_off_ratio[1,2] <- confusion_matrix_XL_ct_off[1,2] / confusion_matrix_XL_ct_off[3,3] * 100
confusion_matrix_XL_ct_off_ratio[2,1] <- confusion_matrix_XL_ct_off[2,1] / confusion_matrix_XL_ct_off[3,3] * 100
confusion_matrix_XL_ct_off_ratio[2,2] <- confusion_matrix_XL_ct_off[2,2] / confusion_matrix_XL_ct_off[3,3] * 100
confusion_matrix_XL_ct_off_ratio[1,3] <- confusion_matrix_XL_ct_off[1,3] / confusion_matrix_XL_ct_off[3,3] * 100 
confusion_matrix_XL_ct_off_ratio[2,3] <- confusion_matrix_XL_ct_off[2,3] / confusion_matrix_XL_ct_off[3,3] * 100
confusion_matrix_XL_ct_off_ratio[3,1] <- confusion_matrix_XL_ct_off[3,1] / confusion_matrix_XL_ct_off[3,3] * 100
confusion_matrix_XL_ct_off_ratio[3,2] <- confusion_matrix_XL_ct_off[3,2] / confusion_matrix_XL_ct_off[3,3] * 100
confusion_matrix_XL_ct_off_ratio[3,3] <- confusion_matrix_XL_ct_off[3,3] / confusion_matrix_XL_ct_off[3,3] * 100
print(confusion_matrix_XL_ct_off_ratio)

confusion_matrix_XL_ct_on_ratio <- confusion_matrix_XL_ct_on
confusion_matrix_XL_ct_on_ratio[1,1] <- confusion_matrix_XL_ct_on[1,1] / confusion_matrix_XL_ct_on[3,3] * 100
confusion_matrix_XL_ct_on_ratio[1,2] <- confusion_matrix_XL_ct_on[1,2] / confusion_matrix_XL_ct_on[3,3] * 100
confusion_matrix_XL_ct_on_ratio[2,1] <- confusion_matrix_XL_ct_on[2,1] / confusion_matrix_XL_ct_on[3,3] * 100
confusion_matrix_XL_ct_on_ratio[2,2] <- confusion_matrix_XL_ct_on[2,2] / confusion_matrix_XL_ct_on[3,3] * 100
confusion_matrix_XL_ct_on_ratio[1,3] <- confusion_matrix_XL_ct_on[1,3] / confusion_matrix_XL_ct_on[3,3] * 100 
confusion_matrix_XL_ct_on_ratio[2,3] <- confusion_matrix_XL_ct_on[2,3] / confusion_matrix_XL_ct_on[3,3] * 100
confusion_matrix_XL_ct_on_ratio[3,1] <- confusion_matrix_XL_ct_on[3,1] / confusion_matrix_XL_ct_on[3,3] * 100
confusion_matrix_XL_ct_on_ratio[3,2] <- confusion_matrix_XL_ct_on[3,2] / confusion_matrix_XL_ct_on[3,3] * 100
confusion_matrix_XL_ct_on_ratio[3,3] <- confusion_matrix_XL_ct_on[3,3] / confusion_matrix_XL_ct_on[3,3] * 100
print(confusion_matrix_XL_ct_on_ratio)


## Confusion Matrix Ratio Results ------------------------------------------

print(confusion_matrix_ratio)
print(confusion_matrix_XL_ratio)

print(confusion_matrix_ct_off_ratio)
print(confusion_matrix_ct_on_ratio)

print(confusion_matrix_XL_ct_off_ratio)
print(confusion_matrix_XL_ct_on_ratio)

# tables
# count number of TRUE in TrackerBlackList
table(merged_data_ct_off_more_info$TrackerBlackList)
table(merged_data_ct_off_more_info$domainType)
table(merged_data_ct_off_more_info$domainType, merged_data_ct_off_more_info$TrackerBlackList)

#count number of TRUE in TrackerBlackList
table(merged_data_ct_on_more_info$TrackerBlackList)
table(merged_data_ct_on_more_info$domainType)
table(merged_data_ct_on_more_info$domainType, merged_data_ct_on_more_info$TrackerBlackList)


#count number of TRUE in TrackerBlackListXL
table(merged_data_ct_off_more_info$TrackerBlackListXL)
table(merged_data_ct_off_more_info$domainType)
table(merged_data_ct_off_more_info$domainType, merged_data_ct_off_more_info$TrackerBlackListXL)

#count number of TRUE in TrackerBlackListXL
table(merged_data_ct_on_more_info$TrackerBlackListXL)
table(merged_data_ct_on_more_info$domainType)
table(merged_data_ct_on_more_info$domainType, merged_data_ct_on_more_info$TrackerBlackListXL)

# 7. Code Leftovers -------------------------------------------------------

## Upload EasyList EasyPrivacy ------------------------

library(httr)
library(readr)

# URL of the blacklist
easylist_easyprivacy_url <- "https://easylist.to/easylist/easyprivacy.txt"

# download the file as plain text
easylist_easyprivacy_txt <- content(GET(easylist_easyprivacy_url), as = "text")

# convert into vector of domains
easylist_easyprivacy_domains <- read_lines(I(easylist_easyprivacy_txt))

# make into a data frame
easylist_easyprivacy_df <- data.frame(domain = easylist_easyprivacy_domains, stringsAsFactors = FALSE)


## Data Cleaning EasyList-----------------------------------------------------------
library(dplyr)
# Clean the domains (remove comments, wildcards, etc.)
easylist_easyprivacy_df_filtered <- easylist_easyprivacy_df %>%
  slice(-1:-18) %>% # remove first 17 lines (header info)
  
  filter(!str_starts(domain, "!")) %>%  # remove comments
  filter(!str_detect(domain, "\\*\\*\\*")) %>%  # remove comments
  filter(!str_starts(domain, "\\@\\@")) %>%  # remove exceptions 
  filter(!str_detect(domain, "\\*")) %>% # remove wildcards
  #filter(!str_starts(domain, "/")) %>%  # remove rules starting with /
  filter(!str_detect(domain, ";")) %>%  # remove anything with ;
  #filter(!str_detect(domain, "?")) %>%  # remove anything with ?
  #filter(!str_detect(domain, "=")) %>%  # remove anything with =
  
  mutate(domain = str_remove_all(domain, "^\\/\\/")) %>%     # remove //
  mutate(domain = str_remove_all(domain, "^\\/")) %>%     # remove /
  mutate(domain = str_remove_all(domain, "^:\\|\\|")) %>% # remove leading :||
  mutate(domain = str_remove_all(domain, "^\\:")) %>%        # remove :
  mutate(domain = str_remove_all(domain, "^\\|\\|")) %>% # remove leading ||
  mutate(domain = str_remove_all(domain, "^\\|")) %>%    # remove leading |
  
  mutate(domain = str_remove_all(domain, "^\\.")) %>%        # remove .
  mutate(domain = str_remove_all(domain, "^\\_\\_")) %>%     # remove _ _
  mutate(domain = str_remove_all(domain, "^\\_")) %>%     # remove _
  mutate(domain = str_remove_all(domain, "^\\?")) %>%     # remove ?
  mutate(domain = str_remove_all(domain, "^\\%")) %>%     # remove %
  mutate(domain = str_remove_all(domain, "^\\&")) %>%        # remove &
  mutate(domain = str_remove_all(domain, "^\\&\\&")) %>%       # remove &&
  mutate(domain = str_remove_all(domain, "^\\/\\/")) %>%     # remove //
  
  mutate(domain = str_remove(domain, "^www\\.")) %>%     # remove leading www.
  mutate(domain = str_remove(domain, "^http\\:\\/\\/")) %>%     # remove leading http://
  mutate(domain = str_remove(domain, "^https\\:\\/\\/")) %>% # remove leading https://
  
  mutate(domain = str_remove_all(domain, "^\\^")) %>%     # remove ^
  mutate(domain = str_remove_all(domain, "^\\&")) %>%     # remove &
  
  mutate(domain = str_remove_all(domain, "\\^$")) %>%    # remove trailing ^
  mutate(domain = str_remove_all(domain, "\\^\\$third-party")) %>%    # remove trailing ^$ third-party
  mutate(domain = str_remove_all(domain, ",domain.*$")) %>%    # remove ,domain ..
  mutate(domain = str_remove_all(domain, ",xmlhttprequest.*$")) %>%    # remove ,xmlhttprequest ..
  mutate(domain = str_remove_all(domain, "/.*$")) %>%     # remove anything after /
  mutate(domain = str_remove_all(domain, "#+.*$")) %>%     # remove anything after #
  mutate(domain = str_remove_all(domain, "\\?.*$")) %>%     # remove anything after ?
  mutate(domain = str_remove_all(domain, "\\_.*$")) %>%     # remove anything after _
  mutate(domain = str_remove_all(domain, "\\=.*$")) %>%     # remove anything after =
  mutate(domain = str_remove_all(domain, "\\$.*$")) %>%     # remove anything after $
  mutate(domain = str_remove_all(domain, "\\~.*$")) %>%     # remove anything after ~
  mutate(domain = str_remove_all(domain, "\\%.*$")) %>%     # remove anything after %
  mutate(domain = str_remove_all(domain, "\\^$")) %>%    # remove trailing ^
  mutate(domain = str_remove_all(domain, "\\.$")) %>%    # remove trailing .
  mutate(domain = str_to_lower(domain)) #%>%               # lowercase
#filter(!str_starts(domain, "[")) %>%  # remove section headers

# remove empty rows
easylist_easyprivacy_df_filtered <- easylist_easyprivacy_df_filtered %>%
  filter(domain != "")

# remove duplicates
easylist_easyprivacy_df_filtered <- easylist_easyprivacy_df_filtered %>%
  distinct(domain, .keep_all = TRUE)


rm(easylist_easyprivacy__domains, easylist_easyprivacy_txt, easylist_easyprivacy_url)

## Web-Parser for Disconnect.me JSON-file -------------------------------------------
#library(jsonlite)

#disconnect_url <- "https://raw.githubusercontent.com/disconnectme/disconnect-tracking-protection/master/services.json"
#disconnect_json <- fromJSON(disconnect_url)

# assuming JSON is a simple array of domains
#disconnect_json <- data.frame(domain = unlist(disconnect_json), stringsAsFactors = FALSE)

# keep only the domain column
#disconnect_json <- disconnect_json %>%
#slice(-1:-1) %>% # remove first line (header info)
#select(domain) %>%
#mutate(domain = clean_domains(domain)) %>%
#filter(domain != "")

rm(disconnect_json)
rm(disconnect_url)

## example code for reading in blacklists and url --------------------------

## upload:
# reading a plain text blacklist
#blacklist <- readLines("tracker_blacklist.txt")

# convert to a data frame
#blacklist_df <- data.frame(domain = blacklist, stringsAsFactors = FALSE)

#library(readr)

# reading a csv file with a column 'domain'
#blacklist_df <- read_csv("tracker_blacklist.csv")

#library(jsonlite)

# reading json file
#blacklist_json <- fromJSON("tracker_blacklist.json")

# assume JSON has a vector/list of domains
#blacklist_df <- data.frame(domain = blacklist_json, stringsAsFactors = FALSE)

#library(dplyr)
#library(stringr)

## Data Cleaning
# lowercase all domains and remove leading "www."
#blacklist_df <- blacklist_df %>%
# mutate(domain = str_to_lower(domain),
#       domain = str_remove(domain, "^www\\."))

#networkActivity_df <- networkActivity_df %>%
#  mutate(domain = str_to_lower(domain),
#        domain = str_remove(domain, "^www\\."))

## Cross-referencing
# find all network activity entries that match blacklist
#matched_activity <- networkActivity_df %>%
# semi_join(blacklist_df, by = "domain")

# check the results
#print(matched_activity, n = 50)

# Or: add a tracker flag to the network activity data
#networkActivity_df <- networkActivity_df %>%
# mutate(is_tracker = domain %in% blacklist_df$domain)

## Summary
#tracker_summary <- networkActivity_df %>%
# filter(is_tracker) %>%
#group_by(domain) %>%
#summarise(total_hits = n(), .groups = "drop") %>%
#arrange(desc(total_hits))

#print(tracker_summary, n = 20)


## Example Scrapping ---------------------------------------------------------------

library(httr)
library(readr)
# txt
# URL of the blacklist
blacklist_url <- "https://example.com/tracker_blacklist.txt"

# download the file as plain text
blacklist_txt <- content(GET(blacklist_url), as = "text")

# convert into vector of domains
blacklist_domains <- read_lines(I(blacklist_txt))

# make into a data frame
blacklist_df <- data.frame(domain = blacklist_domains, stringsAsFactors = FALSE)

## csv
library(readr)

blacklist_url <- "https://example.com/tracker_blacklist.csv"
blacklist_df <- read_csv(blacklist_url)

## json
library(jsonlite)

blacklist_url <- "https://example.com/tracker_blacklist.json"
blacklist_json <- fromJSON(blacklist_url)

# assuming JSON is a simple array of domains
blacklist_df <- data.frame(domain = unlist(blacklist_json), stringsAsFactors = FALSE)

## embedded in html
library(rvest)
library(dplyr)

blacklist_url <- "https://example.com/blacklist-page"

# read page
page <- read_html(blacklist_url)

# extract table or text nodes
blacklist_df <- page %>%
  html_nodes("table") %>%   # adjust selector if necessary
  html_table() %>%
  .[[1]] %>%
  rename(domain = 1)        # assume first column has domains

## mutliple lists
# assuming multiple lists in different data frames: bl1, bl2, bl3
combined_blacklist <- bind_rows(bl1, bl2, bl3) %>%
  distinct(domain, .keep_all = TRUE)

# automation
library(purrr)
library(dplyr)
library(readr)

urls <- c(
  "https://example.com/tracker_blacklist.txt",
  "https://example.com/tracker_blacklist.csv"
)

# function to read text or csv automatically
read_blacklist <- function(url) {
  if (grepl("\\.csv$", url)) {
    df <- read_csv(url)
  } else {
    txt <- read_lines(url)
    df <- data.frame(domain = txt, stringsAsFactors = FALSE)
  }
  return(df)
}

combined_blacklist <- map_df(urls, read_blacklist) %>%
  distinct(domain, .keep_all = TRUE)

### Fin du Script ###

# ### Fin du script ### ---------------------------------------------------
### Fin du script ###