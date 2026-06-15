# code um die "komischen" Zählstelle vs. Messstelle vs. Richtung vs. Knoten bei MIV zu identifizieren und anzuschauen
devtools::load_all()

miv_all <- read_miv_data()

loc_dir <- miv_all  |> arrange(timestamp) |> select(loc_id, loc_name, dir_id, dir_name)  |> distinct()  |> arrange(loc_id) 

# da gibt es Fälle, wo man mehr als zwei Richtungen hat
loc_dir |> 
  count(loc_id) |> 
  filter(n > 2)

miv039001 <- miv_all |> 
  filter(loc_id == "Z039", dir_name == "Albisriederplatz und Hohlstrasse")
# Änderung am 1.1.2018, davor sehr lange fehlend (seit 2016)
# es ändert sich nicht nur die knr sondern auch die Anzahl Detektoren (?) --> besser das nicht als kontinuierliche Zeitreihe fortzuführen?
miv082_buch <- miv_all |> 
  filter(loc_id == "Z082", dir_name == "Bucheggplatz")
# Änderung auch per 1.1.2018, davor ein halbes Jahr keine Daten (auch nicht fehlend)
# Anzahl Detektoren bleibt gleich, könnte man weiterführen?
miv082_albis <- miv_all |> 
  filter(loc_id == "Z082", dir_name == "Albissriederplatz")
# Änderung auch per 1.1.2018, aber Anzahl Detektoren ändert...

miv083 <- miv_all |> 
  filter(loc_id == "Z083", dir_name == "Bucheggplatz")
# auch per 1.1.2018, Anzahl Detektoren bleibt gleich

# das sind die, die in der App als problematisch auftauchen

# zusätzlich gibt es aber auch andere, wo sich bei derselben Messstelle der Knotenpunkt verändert hat, aber die Richtung gleich geblieben ist
n_k_dir <- miv_all  |> 
  select(loc_id, dir_id, dir_name, knummer, kname)  |> 
  distinct()  |> 
  summarise(n_k = length(unique(knummer)), .by = c("loc_id", "dir_id", "dir_name"))
n_k_dir  |> filter(n_k >1)

# die sind für die App nicht problematisch, solange es inhaltlich ok ist, auf die Richtung zu gehen und die veränderten Knoten zu missachten?
miv036001 <- miv_all |> 
  filter(dir_id == "Z036001")
# Wechsel irgendwo rund um Zeile 56000, Mitte Jahr 2018

# Anpassung nötig in read_miv_data
loc_last <- loc_dir  |> summarise(dir_id = last(dir_id), .by = c("loc_id", "dir_name")) 
# diese dir_ids behalten, i.e. miv_all filtern nach diesen dir_ids
# (dann für einmal noch prüfen, ob ich dann die richtigen rausgeworfen habe)
miv_filtered <- miv_all |> 
  filter(dir_id %in% loc_last$dir_id)

miv_removed <- miv_all |> 
  filter(!(dir_id %in% loc_last$dir_id))
