### Network Traffic Analysis Script ###

# Another Overview --------------------------------------------------------

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



# Adding columns with simpler names ---------------------------------------

# Start with bundleID
# Complete Data frame -----------------------------------------------------

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
  

# Same for CT off ------------------------------------------------

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
  
# Same for CT on ------------------------------------------------

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



# Continue with DomainOwner
# Complete Df -------------------------------------------------------------

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


# Creating new column DomainOwnerName ---------------------------------------------------------
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
  select(firstTimeStamp, timeStamp, hits, bundleID, AppName, domain, domainOwner, DomainOwnerName, everything()) -> merged_data_all

# Save df as CSV
write.csv(merged_data_all, "Output/Tables/merged_data_all_df.csv", row.names = TRUE)

# Printing DomainOwnerName summaries ------------------------------------------------------
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

# Filtered Df domain > 10 -------------------------------------------------------------

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


# new df's with domain Owners ---------------------------------------

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

13375 / 100
1718/133.75

# unique number of domains where domainType = 1
length(unique(merged_data_all$domain[merged_data_all$domainType == 1])) # 246

length(unique(merged_data_all$domain[merged_data_all$domainType == 2])) # 4218

# unique number of bundleIDs
length(unique(merged_data_all$bundleID)) # 184

# Next steps: 
# Do the analysis in the CSV file. Use tools like WHOIS, EasyList, DNS lookup, and other tools to find out more about the domains.
# Do the same for CT OFF and CT ON data frames

# Prep for Cross-Referencing DFs with black and white lists ------------------------

library(dplyr)
# remove leading www. from domain in merged_data_all
merged_data_all <- merged_data_all %>%
  mutate(domain = str_remove(domain, "^www\\."))


# example code for reading in blacklists and url --------------------------

## upload:
# reading a plain text blacklist
blacklist <- readLines("tracker_blacklist.txt")

# convert to a data frame
blacklist_df <- data.frame(domain = blacklist, stringsAsFactors = FALSE)

library(readr)

# reading a csv file with a column 'domain'
blacklist_df <- read_csv("tracker_blacklist.csv")

library(jsonlite)

# reading json file
blacklist_json <- fromJSON("tracker_blacklist.json")

# assume JSON has a vector/list of domains
blacklist_df <- data.frame(domain = blacklist_json, stringsAsFactors = FALSE)

library(dplyr)
library(stringr)

## Data Cleaning
# lowercase all domains and remove leading "www."
blacklist_df <- blacklist_df %>%
  mutate(domain = str_to_lower(domain),
         domain = str_remove(domain, "^www\\."))

networkActivity_df <- networkActivity_df %>%
  mutate(domain = str_to_lower(domain),
         domain = str_remove(domain, "^www\\."))

## Cross-referencing
# find all network activity entries that match blacklist
matched_activity <- networkActivity_df %>%
  semi_join(blacklist_df, by = "domain")

# check the results
print(matched_activity, n = 50)

# Or: add a tracker flag to the network activity data
net
workActivity_df <- networkActivity_df %>%
  mutate(is_tracker = domain %in% blacklist_df$domain)

## Summary
tracker_summary <- networkActivity_df %>%
  filter(is_tracker) %>%
  group_by(domain) %>%
  summarise(total_hits = n(), .groups = "drop") %>%
  arrange(desc(total_hits))

print(tracker_summary, n = 20)


# Example Scrapping ---------------------------------------------------------------

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

# EasyList EasyPrivacy ------------------------

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


# Data Cleaning EasyList-----------------------------------------------------------
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


# Ready to run script -----------------------------------------------------

# --- Load required packages ---
library(httr)
library(readr)
library(jsonlite)
library(dplyr)
library(stringr)
library(purrr)

# --- URLs of well-known tracker lists ---
tracker_urls <- list(
  easyprivacy = "https://easylist.to/easylist/easyprivacy.txt",
  stevenblack = "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts",
  disconnect_me = "https://raw.githubusercontent.com/disconnectme/disconnect-tracking-protection/master/services.json"
)

tracker_paths <- list(
  disconnect_me = '/Users/nicolaswaser/New-project-GitHub-first/R/MSA II/Input Data/disconnect.txt'
)

# --- Helper function: clean and normalize domains ---
clean_domains <- function(domains) {
  domains %>%
    str_remove_all("^\\|\\|") %>%  # remove leading ||
    str_remove_all("^\\|") %>%     # remove leading |
    str_remove_all("\\^$") %>%     # remove trailing ^
    str_remove_all("/.*$") %>%     # remove anything after /
    str_to_lower() %>%             # lowercase
    str_remove("^www\\.") %>%         # remove leading www.
    str_remove_all("^\\&") %>%     # remove &
    str_remove_all("^\\.") %>%     # remove .
    str_remove_all("^\\?") %>%     # remove ?
    str_remove_all("^\\_")     # remove _
}

# --- Scraper functions for each source ---
get_easyprivacy <- function(url) {
  txt <- content(GET(url), as = "text")
  raw <- read_lines(I(txt))
  df <- data.frame(domain = raw, stringsAsFactors = FALSE) %>%
    filter(!str_starts(domain, "!")) %>%  # remove comments
    filter(!str_detect(domain, "\\*\\*\\*")) %>%  # remove comments
    filter(!str_starts(domain, "\\@\\@\\|\\|")) %>%  # remove exceptions
    mutate(domain = clean_domains(domain)) %>%
    filter(domain != "")
  return(df)
}

get_stevenblack <- function(url) {
  txt <- content(GET(url), as = "text")
  raw <- read_lines(I(txt))
  df <- data.frame(line = raw, stringsAsFactors = FALSE) %>%
    filter(str_detect(line, "^0\\.0\\.0\\.0\\s+")) %>% # lines with "0.0.0.0 domain"
    mutate(domain = str_remove(line, "^0\\.0\\.0\\.0\\s+")) %>%
    select(domain) %>%
    mutate(domain = clean_domains(domain))
  return(df)
}


## json
library(jsonlite)

disconnect_url <- "https://raw.githubusercontent.com/disconnectme/disconnect-tracking-protection/master/services.json"
disconnect_json <- fromJSON(disconnect_url)

# assuming JSON is a simple array of domains
disconnect_json <- data.frame(domain = unlist(disconnect_json), stringsAsFactors = FALSE)

# keep only the domain column
disconnect_df <- disconnect_json %>%
  select(domain) %>%
  mutate(domain = clean_domains(domain)) %>%
  filter(domain != "")


# --- Download and combine all lists ---
easyprivacy_df <- get_easyprivacy(tracker_urls$easyprivacy)
stevenblack_df <- get_stevenblack(tracker_urls$stevenblack)
#disconnect_df  <- get_disconnect(disconnect_json)

blacklist_df <- bind_rows(
  easyprivacy_df,
  stevenblack_df,
  disconnect_df
) %>%
  distinct(domain, .keep_all = TRUE) %>%
  filter(domain != "" & !is.na(domain))

# --- Preview results ---
head(blacklist_df, 20)
nrow(blacklist_df)


# Cross-referencing with EasyList EasyPrivacy -----------------------------

library(dplyr)
# find all network activity entries that match EasyList EasyPrivacy
#matched_activity_easylist <- merged_data_all %>%
# semi_join(easylist_easyprivacy_df_filtered, by = "domain")
#rm(matched_activity)


# add a tracker flag 
merged_data_all <- merged_data_all %>%
  mutate(EasyListTracker = domain %in% easylist_easyprivacy_df_filtered$domain)

#count number of TRUE in easylist_tracker
table(merged_data_all$EasyListTracker)

table(merged_data_all$domainType)

table(merged_data_all$domainType, merged_data_all$EasyListTracker)


## Summary
tracker_summary_easylist <- merged_data_all %>%
  filter(EasyListTracker) %>%
  group_by(domain) %>%
  summarise(total_hits = n(), .groups = "drop") %>%
  arrange(desc(total_hits))

print(tracker_summary, n = 50)


# Cross-ref stevenblack ---------------------------------------------------

merged_data_all <- merged_data_all %>%
  mutate(stevenblackTracker = domain %in% stevenblack_df$domain)

#count number of TRUE in easylist_tracker
table(merged_data_all$stevenblackTracker)

table(merged_data_all$domainType)

table(merged_data_all$domainType, merged_data_all$stevenblackTracker)


# Cross-ref disconnect ----------------------------------------------------

merged_data_all <- merged_data_all %>%
  mutate(DisconnectTracker = domain %in% disconnect_df$domain)

#count number of TRUE in disconnect_tracker
table(merged_data_all$DisconnectTracker)
table(merged_data_all$domainType)
table(merged_data_all$domainType, merged_data_all$DisconnectTracker)


# Cross-reference all with network activity data --------------------------
merged_data_all <- merged_data_all %>%
  mutate(TrackerBlackList = domain %in% blacklist_df$domain)

#count number of TRUE in easylist_tracker
table(merged_data_all$TrackerBlackList)

table(merged_data_all$domainType)

table(merged_data_all$domainType, merged_data_all$TrackerBlackList)


## Summary
tracker_summary <- merged_data_all %>%
  filter(TrackerBlackList) %>%
  group_by(domain) %>%
  summarise(total_hits = n(), .groups = "drop") %>%
  arrange(desc(total_hits))

print(tracker_summary, n = 50)

# Create new df with only tracker entries
merged_data_all_trackers <- merged_data_all %>%
  filter(TrackerBlackList == TRUE) %>%
  select(firstTimeStamp, timeStamp, hits,
         bundleID, AppName, domainOwner, DomainOwnerName, initiatedType, domainClassification, domain,
         domainType, TrackerBlackList, EasyListTracker, stevenblackTracker, DisconnectTracker)


# Same for CT OFF ---------------------------------------------------------

# remove leading www. from domain in merged_data_ct_off
merged_data_ct_off <- merged_data_ct_off %>%
  mutate(domain = str_remove(domain, "^www\\."))
# Same for CT ON ----------------------------------------------------------

# remove leading www. from domain in merged_data_ct_on
merged_data_ct_on <- merged_data_ct_on %>%
  mutate(domain = str_remove(domain, "^www\\."))

### Fin du Script ###