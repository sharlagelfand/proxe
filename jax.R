# call before finalize.R


########## load in JAX table from Mark #########

# load in JAX metadata
jax.filename <- dir(data_outside_app_dir, pattern = glob2rx("*proxe_jax_key.xlsx"))
if (length(jax.filename) != 1) stop("too few or too many JAX sheets in dropbox")
jax_meta <- read_excel(file.path(data_outside_app_dir, jax.filename), sheet = "Header_Data")
jax_meta <- as.data.frame(jax_meta)

# use that to load in JAX data
jax <- read_excel(file.path(data_outside_app_dir, jax.filename), sheet = 1)
# , col_types =jax_meta$read_excel_type)
jax <- as.data.frame(jax)

######### merge only necessary column with df #########

# filter jax rows and columns
jax2 <- jax[
  jax$proxe_pdx_id != "None" & !is.na(jax$jax_url),
  c("proxe_pdx_id", "jax_pdx_id", "jax_url")
]

# add hyperlink to jax_pdx_id
for (i in 1:nrow(jax2)) {
  jax2$jax_pdx_id[i] <- as.character(
    a(target = "blank", href = jax2$jax_url[i], jax2$jax_pdx_id[i])
  )
}

# filter, rename
jax3 <- jax2[, c("proxe_pdx_id", "jax_pdx_id")]
names(jax3) <- c("proxe_pdx_id", "Distribution via JAX")

# merge with df
df <- merge(df, jax3, by.x = "namenum", by.y = "proxe_pdx_id", all.x = T)
# correct that namenum moved to beginning
df <- moveMe(
  data = df,
  tomove = "namenum",
  where = "last"
)
# move new column in, update cutoff
df <- moveMe(
  data = df,
  tomove = "Distribution via JAX",
  where = "after", ba = names(df)[obInvisRet_ind]
)
obInvisRet_ind <- obInvisRet_ind + 2

# Add to appropriate metadata --- meta? meta3?
if (F) {
  dim(meta)
  dim(meta2)
  dim(jax_meta)
  names(jax_meta)
  names(meta2)
}

# 1. filter down to desired row
jax_meta2 <- jax_meta[jax_meta$Internal_Column_Header == "jax_url", ]
jax_meta2$PRoXe_Column_Header <- "Distribution via JAX"
jax_meta2$Internal_Column_Header <- "Distribution via JAX"

# 2. add other column
jax_meta2$Visible_Invisible_int <- 2

# 3. test
stopifnot(all(names(jax_meta2) == names(meta2)))

# 4. rbind
meta2 <- rbind(meta2, jax_meta2)

# 5. also add to meta_gloss (enables line report)
jax_metag <- jax_meta2
colnames(jax_metag)[colnames(jax_metag) == "Visible_Invisible_int"] <- "In_PRIMAGRAFTS"
jax_metag$In_PRIMAGRAFTS <- 0
meta_gloss <- rbind(meta_gloss, jax_metag)


# update distribution permissions to say 'Available via Jackson Labs'
jax_permission <- "Available via Jackson Labs"
levels(df$`Distribution Permissions`) <- c(
  levels(df$`Distribution Permissions`),
  jax_permission
)

df[!is.na(df$`Distribution via JAX`), ]$`Distribution Permissions` <- jax_permission

# TODO: remove objects to cleanup? They're small. Maybe later.
