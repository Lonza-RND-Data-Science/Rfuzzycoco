# cf https://github.com/rocker-org/devcontainer-features/blob/main/src/r-apt/install.sh### setup r2u


. /usr/local/bin/setup_shared_functions.sh
# check_packages curl ca-certificates
check_packages curl

UBUNTU_CODENAME=noble

### N.B: using LONZA r2u repository!!!!
# cf mirror_r2u
# LONZA_R2U_REPO_BASE=https://dlslaapexplo01.z6.web.core.windows.net/r2u/snapshots
# LONZA_R2U_REPO_BASE=https://dlslaapexplo01.blob.core.windows.net/\$web/r2u/snapshots


# !! the r2u snapshot date to use!!!
DATE=$R2U_SNAPSHOT
test -n "$DATE" || (echo "you must define ARG R2U_SNAPSHOT"; false)

LONZA_R2U_REPO_URL=$LONZA_R2U_REPO_BASE/$UBUNTU_CODENAME/$DATE
# echo "deb [arch=amd64] $LONZA_R2U_REPO_URL ${UBUNTU_CODENAME} main" >/etc/apt/sources.list.d/cranaptlonza.list
curl -fsSL  https://eddelbuettel.github.io/r2u/assets/dirk_eddelbuettel_key.asc | tee -a /etc/apt/trusted.gpg.d/cranapt_key.asc >/dev/null
echo "deb [arch=amd64] https://r2u.stat.illinois.edu/ubuntu ${UBUNTU_CODENAME} main" >/etc/apt/sources.list.d/cranapt.list
# Pinning
cat <<EOF >"/etc/apt/preferences.d/99cranapt"
Package: *
Pin: release o=CRAN-Apt Project
Pin: release l=CRAN-Apt Packages
Pin-Priority: 700
EOF

# do not install APT recommends by default
echo 'APT::Install-Recommends "false";' > /etc/apt/apt.conf.d/90local-no-recommends

echo "r2u configuration DONE."

