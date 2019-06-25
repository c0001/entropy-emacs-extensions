SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null && pwd )"
  SOURCE="$(readlink "$SOURCE")"

  # if $SOURCE was a relative symlink, we need to resolve it relative
  # to the path where the symlink file was located
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" 
done
DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null && pwd )"


# ** variable declaration
submodules_dir=$DIR/elements/submodules
infos_dir=$DIR/elements/info-files

[ ! -d "${infos_dir}" ] && mkdir ${infos_dir}

dash_dir=${submodules_dir}/dash.el
ghub_dir=${submodules_dir}/ghub
magit_dir=${submodules_dir}/magit
magit_popup_dir=${submodules_dir}/magit-popup
webserver_dir=${submodules_dir}/emacs-web-server
witheditor_dir=${submodules_dir}/with-editor
ivy_dir=${submodules_dir}/swiper
nsis_dir=${submodules_dir}/nsis-mode
ew3m_dir=${submodules_dir}/emacs-w3m
transient_dir=${submodules_dir}/transient
usepackage_dir=${submodules_dir}/use-package

# ** commands requested check
Checking_shell ()
{
    required_tools_missing=()
    required_tools=(make emacs makeinfo tex)
    count=0
    for item in ${required_tools[@]}
    do

        if [[ -z $(command -v $item) ]];then
            required_tools_missing[$count]=$item
            let count++
        fi
    done
    if [ -z "$required_tools_missing" ];then
        echo -e "\n-->Shell dependencies satisfied!\n"
    else
        echo "==========Missing dependency=========="
        for item in ${required_tools_missing[@]}
        do
            echo "Missing '$item'."
        done
        echo "Please install them before run this script -v-"
        echo "======================================"
        exit
    fi
}

# ** extract all texi file
# *** utilites
makeinfo_dash ()
{
    cd ${dash_dir}
    cp -v dash.info ${infos_dir}/
}

makeinfo_ghub ()
{
    cd ${ghub_dir}
    make info
    cp -v ghub.info ${infos_dir}/
}

makeinfo_magit ()
{
    cd ${magit_dir}
    make info
    cp -v ./Documentation/magit.info ${infos_dir}/
}

makeinfo_magit_popup ()
{
    cd ${magit_popup_dir}
    make info
    cp -v magit-popup.info ${infos_dir}/
}

makeinfo_webserver ()
{
    cd ${webserver_dir}/doc
    make all
    cp -v web-server.info ${infos_dir}/
}

makeinfo_witheditor ()
{
    cd ${witheditor_dir}
    make info
    cp -v with-editor.info ${infos_dir}/
}

makeinfo_ivy ()
{
    cd ${ivy_dir}/doc
    makeinfo ivy.texi
    cp -v ivy.info ${infos_dir}/
}

makeinfo_nsis ()
{
    cd ${nsis_dir}
    makeinfo nsis-mode.texi
    cp -v nsis-mode.info ${infos_dir}/
}

makeinfo_ew3m ()
{
    cd ${ew3m_dir}
    [ ! -f configure ] && autoconf
    ./configure && make info
    cp -v doc/emacs-w3m.info ${infos_dir}/
}

makeinfo_transient ()
{
    cd ${transient_dir}
    make info
    cp -v docs/transient.info ${infos_dir}/
}

makeinfo_usepackage ()
{
    cd ${usepackage_dir}
    make info
    cp -v use-package.info ${infos_dir}/
}

# *** main
Extract_Info ()
{
    makeinfo_dash
    makeinfo_ew3m
    makeinfo_ghub
    makeinfo_ivy
    makeinfo_magit
    makeinfo_magit_popup
    makeinfo_nsis
    makeinfo_transient
    makeinfo_usepackage
    makeinfo_webserver
    makeinfo_witheditor
}


# ** touch maked indicator
Finished ()
{
    touch $DIR/init
}

# ** main
Checking_shell
Extract_Info
Finished




