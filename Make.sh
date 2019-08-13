EemacsextMake_SOURCE="${BASH_SOURCE[0]}"
while [ -h "$EemacsextMake_SOURCE" ]; do # resolve $EemacsextMake_SOURCE until the file is no longer a symlink
  EemacsextMake_DIR="$( cd -P "$( dirname "$EemacsextMake_SOURCE" )" >/dev/null && pwd )"
  EemacsextMake_SOURCE="$(readlink "$EemacsextMake_SOURCE")"

  # if $EemacsextMake_SOURCE was a relative symlink, we need to resolve it relative
  # to the path where the symlink file was located
  [[ $EemacsextMake_SOURCE != /* ]] && EemacsextMake_SOURCE="$EemacsextMake_DIR/$EemacsextMake_SOURCE" 
done
EemacsextMake_DIR="$( cd -P "$( dirname "$EemacsextMake_SOURCE" )" >/dev/null && pwd )"

# * Code
# ** variable declaration
EemacsextMake_melpadir=${EemacsextMake_DIR}/elements/submodules/melpa
EemacsextMake_upstream_submodules_dir=$EemacsextMake_DIR/elements/submodules/upstream
EemacsextMake_infosdir=$EemacsextMake_DIR/elements/info-files

EemacsextMake_elbatch_modulesparse=${EemacsextMake_DIR}/eemacs-ext-submodules-parse.el
EemacsextMake_elbatch_branchtoggle_batch_file=${EemacsextMake_DIR}/toggle-branch.sh

declare -a EemacsextMake_local_recipes

EemacsextMake_local_recipes_list_file=${EemacsextMake_DIR}/eemacs-ext-recipes-upstream.txt

EemacsextMake_unregular_recipes_dir=${EemacsextMake_DIR}/elements/unregualar-recipes

[ ! -d "${EemacsextMake_infosdir}" ] && mkdir ${EemacsextMake_infosdir}

EemacsextMake_dashdir=${EemacsextMake_upstream_submodules_dir}/dash.el
EemacsextMake_ghubdir=${EemacsextMake_upstream_submodules_dir}/ghub
EemacsextMake_magitdir=${EemacsextMake_upstream_submodules_dir}/magit
EemacsextMake_magitpopupdir=${EemacsextMake_upstream_submodules_dir}/magit-popup
EemacsextMake_webserverdir=${EemacsextMake_upstream_submodules_dir}/emacs-web-server
EemacsextMake_witheditordir=${EemacsextMake_upstream_submodules_dir}/with-editor
EemacsextMake_ivydir=${EemacsextMake_upstream_submodules_dir}/swiper
EemacsextMake_nsisdir=${EemacsextMake_upstream_submodules_dir}/nsis-mode
EemacsextMake_ew3mdir=${EemacsextMake_upstream_submodules_dir}/emacs-w3m
EemacsextMake_transientdir=${EemacsextMake_upstream_submodules_dir}/transient
EemacsextMake_usepackgedir=${EemacsextMake_upstream_submodules_dir}/use-package

declare -A EemacsextMake_initial_failed_index=([0]="common initial with error" [1]="melpa package build with error")
declare -a EemacsextMake_initial_fails_types
declare -a EemacsextMake_initial_failed_mkinfo
declare -a EemacsextMake_initial_failed_mkpkg

# ** commands requested check
EemacsextMake_Checking_shell ()
{
    required_tools_missing=()
    required_tools=(make emacs makeinfo tex emacs)
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

# ** libraries
# *** common_library
EemacsextMake_cl_member_array ()
# member $1 in array $2
{
    local item
    declare _array=("${@:2}")
    local _member
    for item in ${_array[@]}
    do
        [[ $item == "$1" ]] && _member='t'
    done

    if [[ $_member == 't' ]]
    then
        return 0
    else
        return 1
    fi
}

EemacsextMake_wait_seconds ()
{
    secs=$1
    shift
    msg=$@
    while [ $secs -gt 0 ]
    do
        printf "\r\033[KWaiting %.d seconds $msg" $((secs--))
        sleep 1
    done
    echo
}

# *** common usage branch
# **** extract all texi file
# ***** utilites
EemacsextMake_MakeInfo_For_dash ()
{
    cd ${EemacsextMake_dashdir}
    cp -v dash.info ${EemacsextMake_infosdir}/
}

EemacsextMake_MakeInfo_For_ghub ()
{
    cd ${EemacsextMake_ghubdir}
    make info
    [[ $? -ne 0 ]] && EemacsextMake_initial_failed_mkinfo+=("ghub") && exit 1
    cp -v ghub.info ${EemacsextMake_infosdir}/
}

EemacsextMake_MakeInfo_For_magit ()
{
    cd ${EemacsextMake_magitdir}
    make info
    [[ $? -ne 0 ]] && EemacsextMake_initial_failed_mkinfo+=("magit") && exit 1
    cp -v ./Documentation/magit.info ${EemacsextMake_infosdir}/
}

EemacsextMake_MakeInfo_For_magit_popup ()
{
    cd ${EemacsextMake_magitpopupdir}
    make info
    [[ $? -ne 0 ]] && EemacsextMake_initial_failed_mkinfo+=("magit_popup") && exit 1
    cp -v magit-popup.info ${EemacsextMake_infosdir}/
}

EemacsextMake_MakeInfo_For_webserver ()
{
    cd ${EemacsextMake_webserverdir}/doc
    make all
    [[ $? -ne 0 ]] && EemacsextMake_initial_failed_mkinfo+=("emacs-web-server") && exit 1
    cp -v web-server.info ${EemacsextMake_infosdir}/
}

EemacsextMake_MakeInfo_For_witheditor ()
{
    cd ${EemacsextMake_witheditordir}
    make info
    [[ $? -ne 0 ]] && EemacsextMake_initial_failed_mkinfo+=("with-editor") && exit 1
    cp -v with-editor.info ${EemacsextMake_infosdir}/
}

EemacsextMake_MakeInfo_For_ivy ()
{
    cd ${EemacsextMake_ivydir}/doc
    makeinfo ivy.texi
    [[ $? -ne 0 ]] && EemacsextMake_initial_failed_mkinfo+=("ivy") && exit 1
    cp -v ivy.info ${EemacsextMake_infosdir}/
}

EemacsextMake_MakeInfo_For_nsis ()
{
    cd ${EemacsextMake_nsisdir}
    makeinfo nsis-mode.texi
    [[ $? -ne 0 ]] && EemacsextMake_initial_failed_mkinfo+=("nsis") && exit 1
    cp -v nsis-mode.info ${EemacsextMake_infosdir}/
}

EemacsextMake_MakeInfo_For_ew3m ()
{
    cd ${EemacsextMake_ew3mdir}
    [ ! -f configure ] && autoconf
    ./configure && make info
    [[ $? -ne 0 ]] && EemacsextMake_initial_failed_mkinfo+=("emacs w3m") && exit 1
    cp -v doc/emacs-w3m.info ${EemacsextMake_infosdir}/
}

EemacsextMake_MakeInfo_For_transient ()
{
    cd ${EemacsextMake_transientdir}
    make info
    [[ $? -ne 0 ]] && EemacsextMake_initial_failed_mkinfo+=("transient") && exit 1
    cp -v docs/transient.info ${EemacsextMake_infosdir}/
}

EemacsextMake_MakeInfo_For_usepackage ()
{
    cd ${EemacsextMake_usepackgedir}
    make info
    [[ $? -ne 0 ]] && EemacsextMake_initial_failed_mkinfo+=("use-package") && exit 1
    cp -v use-package.info ${EemacsextMake_infosdir}/
}

# **** main
EemacsextMake_Extact_Info ()
{
    echo -e "\e[32m==================================================\e[0m"
    echo -e "\e[33mMake packages infos ...\e[0m \e[34m(common-branch)\e[0m"
    echo -e "\e[32m==================================================\e[0m"
    echo ""

    EemacsextMake_MakeInfo_For_dash
    EemacsextMake_MakeInfo_For_ew3m
    EemacsextMake_MakeInfo_For_ghub
    EemacsextMake_MakeInfo_For_ivy
    EemacsextMake_MakeInfo_For_magit
    EemacsextMake_MakeInfo_For_magit_popup
    EemacsextMake_MakeInfo_For_nsis
    EemacsextMake_MakeInfo_For_transient
    EemacsextMake_MakeInfo_For_usepackage
    EemacsextMake_MakeInfo_For_webserver
    EemacsextMake_MakeInfo_For_witheditor
    [[ ${#EemacsextMake_initial_failed_mkinfo[@]} -ne 0 ]] && EemacsextMake_initial_fails_types+=(0) && exit 1
}

EemacsextMake_Prompt_InfomakeError ()
{
    local item
    local count=1
    echo -e "========================================="
    echo -e "Failed prompts for \e[34mMake info\e[0m"
    echo -e "-----------------------------------------\n"
    for item in ${EemacsextMake_initial_failed_mkinfo[@]}
    do
        echo -e "\e[33m$count:\e[0m '$item' \e[31mmake info failed\e[0m"
        (( count++ ))
    done
}

# *** melpa build branch
EemacsextMake_Make_Melpa_recipes ()
{
    echo -e "\n\e[32mPatching recipes ...\e[0m"
    cd ${EemacsextMake_melpadir}
    make local-recipe
    if [[ $? -ne 0  ]]
    then
       echo -e "\n\e[31mWrong exit code for recipe patch procedur, Abort! \e[0m"
       exit
    else
        echo -e "\n\e[32mAdding unregular recipes ...\e[0m"
        cp -r ${EemacsextMake_unregular_recipes_dir}/* ${EemacsextMake_melpadir}/recipes/
    fi
}

EemacsextMake_GetLocal_ReipeList ()
{
    echo -e "\n\e[32mGenerate entropy emacs upstream recipes ...\e[0m"
    local IFS=$'\n'
    local item
    declare -a temp_list=$(cat ${EemacsextMake_local_recipes_list_file})
    for item in ${temp_list[@]}
    do
        EemacsextMake_local_recipes+=($item)
    done
}

EemacsextMake_BuildRecipes ()
{
    echo -e "\e[32m============================================================\e[0m"
    echo -e "\e[33mPackage build from local melpa ...\e[0m \e[34m(melpa branch)\e[0m"
    echo -e "\e[32m============================================================\e[0m"
    echo ""
    
    local which
    local choice
    EemacsextMake_Make_Melpa_recipes
    EemacsextMake_GetLocal_ReipeList
    cd ${EemacsextMake_melpadir}
    for which in ${EemacsextMake_local_recipes[@]}
    do
        echo -e "\e[32m😼: building '$which'...\e[0m\n"
        make recipes/$which
        if [[ $? -ne 0 ]]
        then
            EemacsextMake_initial_failed_mkpkg+=($which)
            read -p $'\e[31mPackage building task\e[0m \e[31mfailed, continue next task?\e[0m ' choice;
            [[ $choice != 'y' ]] && exit 1
        fi
    done
    [[ ${#EemacsextMake_initial_failed_mkpkg[@]} -ne  0 ]] && EemacsextMake_initial_fails_types+=(1) && exit 1
    # initialize packages archives contents used for package.el builtin with emacs
    make index
    # recovery the recipes patch
    cd ${EemacsextMake_melpadir} && git checkout recipes
}

EemacsextMake_RecipeBuild_ErrorPrompts ()
{
    local item
    local count=1
    echo -e "========================================="
    echo -e "Failed prompts for \e[34mPackage building\e[0m"
    echo -e "-----------------------------------------\n"
    for item in ${EemacsextMake_initial_failed_mkpkg[@]}
    do
        echo -e "\e[33m$count:\e[0m '$item' \e[31mpackage build failed\e[0m"
        (( count++ ))
    done
}


# ** touch maked indicator
EemacsextMake_Finished ()
{

    if [[ ${#EemacsextMake_initial_fails_types[@]} -ne 0 ]]
    then
        [[ $(EemacsextMake_cl_member_array 0 "${EemacsextMake_initial_fails_types[@]}") == 0 ]] \
            && EemacsextMake_Prompt_InfomakeError

        echo ""
        
        [[ $(EemacsextMake_cl_member_array 1 "${EemacsextMake_initial_fails_types[@]}") == 0 ]] \
            && EemacsextMake_RecipeBuild_ErrorPrompts
    else
    touch $EemacsextMake_DIR/init
    fi
}

# ** main
[[ -f $EemacsextMake_DIR/init ]] && rm ${EemacsextMake_DIR}/init

echo -e "\e[32mTidy up working directory ...\e[0m"
EemacsextMake_wait_seconds 10 "\e[33m[you can cancel this procedure in 10s]\e[0m ..."
cd ${EemacsextMake_DIR}
git submodule deinit --all -f
[[ $? -ne 0 ]] && exit
git submodule update --init
[[ $? -ne 0 ]] && exit

echo ""

echo -e "\e[32mToggle submodule branch ...\e[0m"
EemacsextMake_wait_seconds 10 "\e[33m[you can cancel this procedure in 10s]\e[0m ..."
cd ${EemacsextMake_DIR}
emacs -Q --batch -l ${EemacsextMake_elbatch_modulesparse} --eval "(eemacs-ext/ggsh--gen-branch-toggle-cmd)"
[[ $? -ne 0 ]] && exit
cd ${EemacsextMake_DIR}
bash ${EemacsextMake_elbatch_branchtoggle_batch_file}
[[ $? -ne 0 ]] && exit

echo ""

echo -e "\e[32mMain process starting ....\e[0m"
echo -e "=====================================\n"
cd ${EemacsextMake_DIR}
EemacsextMake_Checking_shell
echo ""
EemacsextMake_Extact_Info
echo ""
EemacsextMake_BuildRecipes

EemacsextMake_Finished





