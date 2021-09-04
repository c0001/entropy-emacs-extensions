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
EemacsextMake_melpadir="${EemacsextMake_DIR}"/elements/submodules/melpa
EemacsextMake_elpadir="${EemacsextMake_DIR}"/elements/submodules/elpa
EemacsextMake_upstream_submodules_dir="$EemacsextMake_DIR"/elements/submodules/upstream

EemacsextMake_elbatch_modulesparse_elisp_file="${EemacsextMake_DIR}"/eemacs-ext-submodules-parse.el
EemacsextMake_elbatch_branchtoggle_bashscript_file="${EemacsextMake_DIR}"/annex/bin/submodules-common-toggle-branch.sh

declare -a EemacsextMake_local_recipes

EemacsextMake_local_recipes_list_file="${EemacsextMake_DIR}"/eemacs-ext-recipes-upstream.txt

EemacsextMake_unregular_recipes_dir="${EemacsextMake_DIR}"/elements/unregualar-recipes

EemacsextMake_error_log_host="$EemacsextMake_DIR"/build_log
[[ -d "$EemacsextMake_error_log_host" ]] && rm -rf "$EemacsextMake_error_log_host"
mkdir -p "$EemacsextMake_error_log_host"

# The faild prompt function host list, if empty after the make
# procedure indicates there's non error during make. Any item in this
# list is an function to prompt specified error log
declare -a EemacsextMake_initial_fails_types

declare -a EemacsextMake_initial_failed_mkpkg
EemacsextMake_initial_failed_mkpkg_output_file="$EemacsextMake_error_log_host"/mkpkg.log

# ** commands requested check
EemacsextMake_Checking_shell ()
{
    required_tools_missing=()
    required_tools=(make emacs makeinfo tex git less)
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
    for item in "${_array[@]}"
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

# *** melpa build branch
EemacsextMake_Make_Melpa_recipes ()
{
    echo -e "\n\e[32mPatching recipes ...\e[0m"
    cd "${EemacsextMake_melpadir}"
    make local-recipe
    if [[ $? -ne 0  ]]
    then
       echo -e "\n\e[31mWrong exit code for recipe patch procedur, Abort! \e[0m"
       exit
    else
        echo -e "\n\e[32mAdding unregular recipes ...\e[0m"
        cp -rf "${EemacsextMake_unregular_recipes_dir}"/* "${EemacsextMake_melpadir}"/recipes/
    fi
}

EemacsextMake_GetLocal_ReipeList ()
{
    echo -e "\n\e[32mGenerate entropy emacs upstream recipes ...\e[0m"
    local item
    while IFS= read -r item
    do
        [[ ! -z "$item" ]] && EemacsextMake_local_recipes+=("$item")
    done < "${EemacsextMake_local_recipes_list_file}"
}

EemacsextMake_BuildRecipes ()
{
    echo -e "\e[32m============================================================\e[0m"
    echo -e "\e[33mPackage build from local melpa ...\e[0m \e[34m(melpa branch)\e[0m"
    echo -e "\e[32m============================================================\e[0m"
    echo ""

    local which
    local choice
    local count=1
    local recipeslen

    EemacsextMake_Make_Melpa_recipes
    EemacsextMake_GetLocal_ReipeList

    recipeslen="${#EemacsextMake_local_recipes[@]}"

    cd "${EemacsextMake_melpadir}"

    for which in "${EemacsextMake_local_recipes[@]}"
    do
        echo -e "\e[32m😼: building '$which'...\e[0m\n\e[34m--->[remains:\e[0m \e[33m$(( $recipeslen - $count ))\e[0m \e[34m]\e[0m\n"
        make recipes/"$which"
        if [[ $? -ne 0 ]]
        then
            EemacsextMake_initial_failed_mkpkg+=("$which")
            read -p $'\e[31mPackage building task\e[0m \e[31mfailed, continue next task?\e[0m ' choice;
            [[ $choice = 'y' ]] || [[ $choice = 'yes' ]] || [[ $choice = 'YES' ]] || exit 1
        fi

        count=$(($count + 1 ))
    done
    if [[ "${#EemacsextMake_initial_failed_mkpkg[@]}" -ne  0 ]]
    then
        EemacsextMake_initial_fails_types+=('EemacsextMake_RecipeBuild_ErrorPrompts')
    else
        # initialize packages archives contents used for package.el builtin with emacs
        make index
    fi
    # recovery the recipes patch
    cd "${EemacsextMake_melpadir}" && git checkout recipes && git clean -xfd recipes
}

EemacsextMake_RecipeBuild_ErrorPrompts ()
{
    local item
    local count=1
    local error_str
    local error_str_nonpropertize
    echo -e "========================================="
    echo -e "Failed prompts for \e[34mPackage building\e[0m"
    echo -e "-----------------------------------------\n"
    for item in "${EemacsextMake_initial_failed_mkpkg[@]}"
    do
        error_str="\e[33m$count:\e[0m '$item' \e[31mpackage build failed\e[0m"
        error_str_nonpropertize="$count: '$item' package build failed"
        echo -e "$error_str"
        echo "$error_str_nonpropertize" >> "$EemacsextMake_initial_failed_mkpkg_output_file"

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
    cd "${EemacsextMake_elpadir}"
    git worktree prune
    git branch -D elpa-admin
    make setup
    make worktrees
    make build-all
}

# *** maintaining

EemacsextMake_get_submodule_update_suggestion ()
{
    local logfile="${EemacsextMake_DIR}"/annex/submodules-update-suggestion.org
    echo ""
    echo -e "\e[32mGet submodule update suggestions ...\e[0m"
    emacs --batch -q -l "$EemacsextMake_elbatch_modulesparse_elisp_file" \
          --eval "(eemacs-ext/ggsh-gen-submodule-update-suggestion)"
    if [[ -f "$logfile" ]]
    then
        less "$logfile"
    fi
}

# ** touch maked indicator
EemacsextMake_Finished ()
{
    local func
    if [[ "${#EemacsextMake_initial_fails_types[@]}" -ne 0 ]]
    then
        for func in "${EemacsextMake_initial_fails_types[@]}"
        do
            if [[ ! -z $func ]]
            then
                $func
            fi
        done
        exit 1
    else
        touch "$EemacsextMake_DIR"/init
    fi
}

# ** main
EemacsextMake_Main_Remove_InitFlag ()
{
    [[ -f "$EemacsextMake_DIR"/init ]] && rm "${EemacsextMake_DIR}"/init
}

EemacsextMake_Main_Tidyup_WorkTree ()
{
    local target_path=$1
    if [[ ! -z "${target_path}" ]]
    then
        echo -e "\e[32mTidy up working directory \e[33m'${target_path}'\e[0m ...\e[0m"
    else
        echo -e "\e[32mTidy up working directory ...\e[0m \e[31m[⚠ ALL]\e[0m"
    fi
    EemacsextMake_wait_seconds 10 "\e[33m[you can cancel this procedure in 10s]\e[0m ..."
    cd "${EemacsextMake_DIR}"
    if [[ -z "${target_path}" ]]
    then
        git submodule deinit --all -f
    else
        git submodule deinit -f "${target_path}"
    fi
    [[ $? -ne 0 ]] && exit
    if [[ -z "${target_path}" ]]
    then
        git submodule update --init
    else
        git submodule update --init "${target_path}"
    fi
    [[ $? -ne 0 ]] && exit
    echo ""
}

EemacsextMake_Main_Toggle_SubBranch ()
{
    [[ -f "${EemacsextMake_elbatch_branchtoggle_bashscript_file}" ]] && rm -f "${EemacsextMake_elbatch_branchtoggle_bashscript_file}"
    local recovery=$1
    cd "${EemacsextMake_DIR}"
    if [[ -z $recovery ]]
    then
        echo -e "\e[32mToggle submodule branch ...\e[0m"
        emacs -Q --batch -l "${EemacsextMake_elbatch_modulesparse_elisp_file}" --eval "(eemacs-ext/ggsh-gen-submodules-common-branch-toggle-bash-script)"
    else
        emacs -Q --batch -l "${EemacsextMake_elbatch_modulesparse_elisp_file}" --eval "(eemacs-ext/ggsh-gen-submodules-common-branch-toggle-bash-script t)"
    fi
    [[ $? -ne 0 ]] && exit
    cd "${EemacsextMake_DIR}"
    if [[ -f "${EemacsextMake_elbatch_branchtoggle_bashscript_file}" ]]
    then
        bash "${EemacsextMake_elbatch_branchtoggle_bashscript_file}"
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
    cd "${EemacsextMake_DIR}"
    git submodule foreach \
        "if test ! -z \"\$(git for-each-ref --format=\"%(refname:short)\" refs/heads/EemacsExtTempo-*)\" ;then \
            git for-each-ref --format=\"%(refname:short)\" refs/heads/EemacsExtTempo-* | xargs git branch -D -f; \
         else \
            echo \"\\e[32mNone tempo branches\\e[0m\"; \
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
    cd "${EemacsextMake_DIR}"
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
    echo -e "- 'build-recipes':       build all recipes (it will doing 'patch-recipes' firstly)"
    echo -e "- 'build-elpa_recipes':  build elpa recips (which tracking with https://git.savannah.gnu.org/cgit/emacs/elpa.git)"
    echo -e "- 'build-eemacs_recipes: build eemacs-packages'"
    echo -e "- 'clean':               clean all stuffs generated (git clean and deinit)"
    echo -e "- 'all':                 build project"
    echo -e ""
    echo -e "--------------------------------maintainability------------------------------------"
    echo -e "- 'sb-upsuggest':        get submodule update suggestions (for *maintainer* only)"
}

EemacsextMake_Main_Choice ()
{
    case $1 in
        init) cd "$EemacsextMake_DIR" && git clean -xfd .
              EemacsextMake_Main_Tidyup_WorkTree ;;

        tidy-branches) cd "$EemacsextMake_DIR" && git clean -xfd .
                       EemacsextMake_Main_Tidyup_TempBranches ;;

        toggle-branches) cd "$EemacsextMake_DIR" && git clean -xfd .
                         EemacsextMake_Main_Toggle_SubBranch ;;

        patch-recipes) EemacsextMake_Main_Tidyup_WorkTree "$(EemacsextMake_GetRepoPath ${EemacsextMake_melpadir})"
                       EemacsextMake_Make_Melpa_recipes ;;

        build-recipes) EemacsextMake_Main_Choice init
                       EemacsextMake_Main_Choice toggle-branches
                       EemacsextMake_BuildRecipes ;;

        build-elpa_recipes) EemacsextMake_Main_Tidyup_WorkTree "$(EemacsextMake_GetRepoPath ${EemacsextMake_elpadir})"
                            EemacsextMake_BuildElpa_Recipes ;;

        build-eemacs_recipes)
            echo -e "\e[31mOff-line, all of eemacs packages have been migrated into eemacs self.\e[0m"
            # EemacsextMake_Main_Tidyup_WorkTree elements/submodules/eemacs-packages
            # EemacsextMake_BuildRecipes eemacs
            ;;

        clean) cd "$EemacsextMake_DIR" && git clean -xfd . && git submodule deinit --all -f ;;

        all) EemacsextMake_Main_All ;;

        # maintainability part
        sb-upsuggest) EemacsextMake_Main_Choice init
                      EemacsextMake_get_submodule_update_suggestion ;;

        # Otherwise
        *) EemacsextMake_Main_Help ;;
    esac
}

# * provide

EemacsextMake_Checking_shell

cd "${EemacsextMake_DIR}"

EemacsextMake_Main_Choice "$1"
