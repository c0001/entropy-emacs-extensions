## eemacs-ext Make.sh --- The core make procedure for eemacs-ext 

# * Copyright (C) 2019  Entropy
# #+BEGIN_EXAMPLE
# Author:        Entropy <bmsac0001@gmail.com>
# Maintainer:    Entropy <bmsac001@gmail.com>
# URL:           https://github.com/c0001/entropy-emacs-extensions/blob/master/Make.sh
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# #+END_EXAMPLE
# * Commentary:
#
# The main procedure of making eemacs-ext project, type 
# 'bash Make.sh' to see the usage prompts.
#
# * Code
# ** preface
EemacsextMake_SOURCE="${BASH_SOURCE[0]}"
while [ -h "$EemacsextMake_SOURCE" ]; do # resolve $EemacsextMake_SOURCE until the file is no longer a symlink
  EemacsextMake_DIR="$( cd -P "$( dirname "$EemacsextMake_SOURCE" )" >/dev/null && pwd )"
  EemacsextMake_SOURCE="$(readlink "$EemacsextMake_SOURCE")"

  # if $EemacsextMake_SOURCE was a relative symlink, we need to resolve it relative
  # to the path where the symlink file was located
  [[ $EemacsextMake_SOURCE != /* ]] && EemacsextMake_SOURCE="$EemacsextMake_DIR/$EemacsextMake_SOURCE" 
done
EemacsextMake_DIR="$( cd -P "$( dirname "$EemacsextMake_SOURCE" )" >/dev/null && pwd )"

EemacsextMake_dir_nontrail_slash ()
# judge path string $1 trailing slash existed status, and return the
# justice code 0 for non-trailing-slash and 1 for otherwise, if $2
# equal to 'lame' than return the trailing slash lamed path string.
{
    local trail_slash_P=$([[ ! -z $(echo $1 | grep -P "/$") ]] && echo yes || echo no)
    if [[ ${trail_slash_P} == "yes" ]]
    then
        if [[ $2 == "lame" ]]
        then
            echo $(echo $1 | sed 's/\/$//' -)
        else
            echo 1
        fi
    else
        if [[ $2 == "lame" ]]
        then
            echo $1
        else
            echo 0
        fi
    fi
}

EemacsextMake_DIR="$(EemacsextMake_dir_nontrail_slash ${EemacsextMake_DIR} lame)"

# ** variable declaration
EemacsextMake_melpadir=${EemacsextMake_DIR}/elements/submodules/melpa
EemacsextMake_elpadir=${EemacsextMake_DIR}/elements/submodules/elpa
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

EemacsextMake_GetRepoPath ()
{
    echo "$1" | sed -E "s|${EemacsextMake_DIR}/||"
}

# *** common usage branch
# **** extract all texi file
# ***** utilites
EemacsextMake_MakeInfo_For_dash ()
{
    cd ${EemacsextMake_dashdir}
    cp -v dash.info ${EemacsextMake_infosdir}/
    git clean -xfd .
}

EemacsextMake_MakeInfo_For_ghub ()
{
    cd ${EemacsextMake_ghubdir}
    make info
    if [[ $? -ne 0 ]]
    then
        EemacsextMake_initial_failed_mkinfo+=("ghub")
    else
        cp -v ghub.info ${EemacsextMake_infosdir}/
        git clean -xfd .
    fi
}

EemacsextMake_MakeInfo_For_magit ()
{
    cd ${EemacsextMake_magitdir}
    make info
    if [[ $? -ne 0 ]]
    then
        EemacsextMake_initial_failed_mkinfo+=("magit")
    else
        cp -v ./Documentation/magit.info ${EemacsextMake_infosdir}/
        git clean -xfd .
    fi
}

EemacsextMake_MakeInfo_For_magit_popup ()
{
    cd ${EemacsextMake_magitpopupdir}
    make info
    if [[ $? -ne 0 ]]
    then
        EemacsextMake_initial_failed_mkinfo+=("magit_popup")
    else
        cp -v magit-popup.info ${EemacsextMake_infosdir}/
        git clean -xfd .
    fi
}

EemacsextMake_MakeInfo_For_webserver ()
{
    cd ${EemacsextMake_webserverdir}/doc
    make all
    if [[ $? -ne 0 ]]
    then
        EemacsextMake_initial_failed_mkinfo+=("emacs-web-server")
    else
        cp -v web-server.info ${EemacsextMake_infosdir}/
        git clean -xfd .
    fi
}

EemacsextMake_MakeInfo_For_witheditor ()
{
    cd ${EemacsextMake_witheditordir}
    make info
    if [[ $? -ne 0 ]]
    then
        EemacsextMake_initial_failed_mkinfo+=("with-editor")
    else
        cp -v with-editor.info ${EemacsextMake_infosdir}/
        git clean -xfd .
    fi
}

EemacsextMake_MakeInfo_For_ivy ()
{
    cd ${EemacsextMake_ivydir}/doc
    makeinfo ivy.texi
    if [[ $? -ne 0 ]]
    then
        EemacsextMake_initial_failed_mkinfo+=("ivy")
    else
        cp -v ivy.info ${EemacsextMake_infosdir}/
        git clean -xfd .
    fi
}

EemacsextMake_MakeInfo_For_nsis ()
{
    cd ${EemacsextMake_nsisdir}
    makeinfo nsis-mode.texi
    if [[ $? -ne 0 ]]
    then
        EemacsextMake_initial_failed_mkinfo+=("nsis")
    else
        cp -v nsis-mode.info ${EemacsextMake_infosdir}/
        git clean -xfd .
    fi
}

EemacsextMake_MakeInfo_For_ew3m ()
{
    cd ${EemacsextMake_ew3mdir}
    [ ! -f configure ] && autoconf
    ./configure && make info
    if [[ $? -ne 0 ]]
    then
        EemacsextMake_initial_failed_mkinfo+=("emacs w3m")
    else
        cp -v doc/emacs-w3m.info ${EemacsextMake_infosdir}/
        git clean -xfd .
    fi
}

EemacsextMake_MakeInfo_For_transient ()
{
    cd ${EemacsextMake_transientdir}
    make info
    if [[ $? -ne 0 ]]
    then
        EemacsextMake_initial_failed_mkinfo+=("transient")
    else
        cp -v docs/transient.info ${EemacsextMake_infosdir}/
        git clean -xfd .
    fi
}

EemacsextMake_MakeInfo_For_usepackage ()
{
    cd ${EemacsextMake_usepackgedir}
    make info
    if [[ $? -ne 0 ]]
    then
        EemacsextMake_initial_failed_mkinfo+=("use-package")
    else
        cp -v use-package.info ${EemacsextMake_infosdir}/
        git clean -xfd .
    fi
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
    [[ ${#EemacsextMake_initial_failed_mkinfo[@]} -ne 0 ]] && EemacsextMake_initial_fails_types+=(0)
}

EemacsextMake_Infomake_ErrorPrompts ()
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
        [[ ! -z $item ]] && EemacsextMake_local_recipes+=($item)
    done
}

EemacsextMake_BuildRecipes ()
{
    echo -e "\e[32m============================================================\e[0m"
    echo -e "\e[33mPackage build from local melpa ...\e[0m \e[34m(melpa branch)\e[0m"
    echo -e "\e[32m============================================================\e[0m"
    echo ""

    local full=nil
    local which
    local choice
    local count=1
    local recipeslen

    [[ -z $1 ]] && full=t

    EemacsextMake_Make_Melpa_recipes
    EemacsextMake_GetLocal_ReipeList

    recipeslen=${#EemacsextMake_local_recipes[@]}

    cd ${EemacsextMake_melpadir}
    
    for which in ${EemacsextMake_local_recipes[@]}
    do
        if [[ $full == 't' ]] || [[ ! -z $(echo $which | grep -P "^entropy-") ]]
        then
            echo -e "\e[32m😼: building '$which'...\e[0m\n\e[34m--->[remains:\e[0m \e[33m$(( $recipeslen - $count ))\e[0m \e[34m]\e[0m\n"
            make recipes/$which
            if [[ $? -ne 0 ]]
            then
                EemacsextMake_initial_failed_mkpkg+=($which)
                read -p $'\e[31mPackage building task\e[0m \e[31mfailed, continue next task?\e[0m ' choice;
                [[ $choice != 'y' ]] && exit 1
            fi
        fi
        count=$(($count + 1 ))
    done
    if [[ ${#EemacsextMake_initial_failed_mkpkg[@]} -ne  0 ]]
    then
        EemacsextMake_initial_fails_types+=(1)
        EemacsextMake_RecipeBuild_ErrorPrompts
    else
        # initialize packages archives contents used for package.el builtin with emacs
        make index
    fi
    # recovery the recipes patch
    cd ${EemacsextMake_melpadir} && git checkout recipes && git clean -xfd recipes
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


# *** elpa build branch

EemacsextMake_BuildElpa_Recipes ()
{
    echo ""
    echo -e "\e[33m==================================================\e[0m"
    echo -e "\e[32mBuilding elpa recipes ...\e[0m"
    echo -e "\e[33m==================================================\e[0m"
    cd ${EemacsextMake_elpadir}
    make archive
}

# ** touch maked indicator
EemacsextMake_Finished ()
{

    if [[ ${#EemacsextMake_initial_fails_types[@]} -ne 0 ]]
    then
        [[ $(EemacsextMake_cl_member_array 0 "${EemacsextMake_initial_fails_types[@]}") == 0 ]] \
            && EemacsextMake_Infomake_ErrorPrompts

        echo ""
        
        [[ $(EemacsextMake_cl_member_array 1 "${EemacsextMake_initial_fails_types[@]}") == 0 ]] \
            && EemacsextMake_RecipeBuild_ErrorPrompts
    else
    touch $EemacsextMake_DIR/init
    fi
}

# ** main
EemacsextMake_Main_Remove_InitFlag ()
{
    [[ -f $EemacsextMake_DIR/init ]] && rm ${EemacsextMake_DIR}/init
}

EemacsextMake_Main_Tidyup_WorkTree ()
{
    local target_path=$1
    if [[ ! -z ${target_path} ]]
    then
        echo -e "\e[32mTidy up working directory \e[33m'${target_path}'\e[0m ...\e[0m"
    else
        echo -e "\e[32mTidy up working directory ...\e[0m"
    fi
    EemacsextMake_wait_seconds 10 "\e[33m[you can cancel this procedure in 10s]\e[0m ..."
    cd ${EemacsextMake_DIR}
    if [[ -z ${target_path} ]]
    then
        git submodule deinit --all -f
    else
        git submodule deinit -f ${target_path}
    fi
    [[ $? -ne 0 ]] && exit
    git submodule update --init ${target_path}
    [[ $? -ne 0 ]] && exit
    echo ""
}

EemacsextMake_Main_Toggle_SubBranch ()
{
    [[ -f ${EemacsextMake_elbatch_branchtoggle_batch_file} ]] && rm -f ${EemacsextMake_elbatch_branchtoggle_batch_file}
    local recovery=$1
    cd ${EemacsextMake_DIR}
    if [[ -z $recovery ]]
    then
        echo -e "\e[32mToggle submodule branch ...\e[0m"
        emacs -Q --batch -l ${EemacsextMake_elbatch_modulesparse} --eval "(eemacs-ext/ggsh--gen-branch-toggle-cmd)"
    else
        emacs -Q --batch -l ${EemacsextMake_elbatch_modulesparse} --eval "(eemacs-ext/ggsh--gen-branch-toggle-cmd t)"
    fi
    [[ $? -ne 0 ]] && exit
    cd ${EemacsextMake_DIR}
    if [[ -f ${EemacsextMake_elbatch_branchtoggle_batch_file} ]]
    then
        bash ${EemacsextMake_elbatch_branchtoggle_batch_file}
        [[ $? -ne 0 ]] && exit
    else
        echo -e "\e[31mPlease initialize submodules first!\e[0m"
        exit
    fi
    echo ""
}

EemacsextMake_Main_Tidyup_TempBranches ()
{
    echo -e "\e[32mDelete temporal branches ...\e[0m"
    EemacsextMake_Main_Toggle_SubBranch t
    cd ${EemacsextMake_DIR}
    git submodule foreach \
        "if [[ ! -z \$(git for-each-ref --format=\"%(refname:short)\" refs/heads/EemacsExtTempo-*) ]];then
            git for-each-ref --format=\"%(refname:short)\" refs/heads/EemacsExtTempo-* | xargs git branch -D -f;
         else
            echo -e \"\e[32mNone tempo branches\e[0m\"
         fi;"
    echo ""
}

EemacsextMake_Main_All ()
{
    EemacsextMake_Main_Remove_InitFlag
    EemacsextMake_Main_Tidyup_WorkTree
    EemacsextMake_Main_Tidyup_TempBranches
    EemacsextMake_Main_Toggle_SubBranch
    echo -e "\e[32mMain process starting ....\e[0m"
    echo -e "=====================================\n"
    cd ${EemacsextMake_DIR}
    echo ""
    EemacsextMake_Extact_Info
    echo ""
    EemacsextMake_BuildRecipes
    EemacsextMake_BuildElpa_Recipes
    EemacsextMake_Finished
}

EemacsextMake_Main_Help ()
{
    echo -e "Valid argument are:"
    echo -e ""
    echo -e "- 'init':                tidy up working directory and initialize submodules"
    echo -e "- 'tidy-branches':       remove all temporal banches making by 'toggle-branches'"
    echo -e "- 'toggle-branches':     toggle working branchs to temporal one which named with prefix '[entropy-emacs]-'"
    echo -e "- 'patch-recipes':       patch recipes adapting for entropy-emacs"
    echo -e "- 'build_recipes':       build all recipes (it will doing 'patch-recipes' firstly)"
    echo -e "- 'build_elpa_recipes':  build elpa recips (which tracking with https://git.savannah.gnu.org/cgit/emacs/elpa.git)"
    echo -e "- 'build_eemacs_recipes: build eemacs-packages'"
    echo -e "- 'make-infos':          make up all submodules texinfo doc"
    echo -e "- 'all':                 build project"
}

EemacsextMake_Main_Choice ()
{
    case $1 in
        init) EemacsextMake_Main_Tidyup_WorkTree ;;
        tidy-branches) EemacsextMake_Main_Tidyup_TempBranches ;;
        toggle-branches) EemacsextMake_Main_Toggle_SubBranch ;;
        patch-recipes) EemacsextMake_Main_Tidyup_WorkTree "$(EemacsextMake_GetRepoPath ${EemacsextMake_melpadir})"
                       EemacsextMake_Make_Melpa_recipes ;;
        build_recipes) EemacsextMake_Main_Choice init
                       EemacsextMake_Main_Choice toggle-branches
                       EemacsextMake_BuildRecipes ;;
        build_elpa_recipes) EemacsextMake_Main_Tidyup_WorkTree "$(EemacsextMake_GetRepoPath ${EemacsextMake_elpadir})"
                            EemacsextMake_BuildElpa_Recipes ;;
        build_eemacs_recipes) EemacsextMake_Main_Tidyup_WorkTree elements/submodules/eemacs-packages
                              EemacsextMake_BuildRecipes eemacs ;;
        make-infos) EemacsextMake_Main_Tidyup_WorkTree "$(EemacsextMake_GetRepoPath ${EemacsextMake_upstream_submodules_dir})"
                    EemacsextMake_Extact_Info ;;
        all) EemacsextMake_Main_All ;;
        *) EemacsextMake_Main_Help ;;
    esac
}

EemacsextMake_Checking_shell

EemacsextMake_Main_Choice $1
