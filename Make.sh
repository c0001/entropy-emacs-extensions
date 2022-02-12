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

EemacsextMake_MainCommand=''

EemacsextMake_melpadir="${EemacsextMake_DIR}"/elements/submodules/melpa
EemacsextMake_elpadir="${EemacsextMake_DIR}"/elements/submodules/elpa
EemacsextMake_upstream_submodules_dir="$EemacsextMake_DIR"/elements/submodules/upstream

EemacsextMake_elbatch_modulesparse_elisp_file="${EemacsextMake_DIR}"/eemacs-ext-submodules-parse.el
EemacsextMake_elbatch_branchtoggle_bashscript_file="${EemacsextMake_DIR}"/annex/bin/submodules-common-toggle-branch.sh

declare -a EemacsextMake_local_recipes

EemacsextMake_local_recipes_list_file="${EemacsextMake_DIR}"/eemacs-ext-recipes-upstream.txt

EemacsextMake_unregular_recipes_dir="${EemacsextMake_DIR}"/elements/unregualar-recipes

EemacsextMake_error_log_host="$EemacsextMake_DIR"/build_log

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
    required_tools=(make emacs makeinfo tex git less xargs find tar xz date)
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

error_msg ()
{
    if [ ! $? -eq 0 ]
    then
        echo -e "\e[31m$1\e[0m"
        exit 1
    fi
}

warn_msg ()
{
    echo -e "\e[33m$1\e[0m"
}

do_msg ()
{
    echo -e "\e[34mDoing $1 ...\e[0m"
}

date_str_get ()
{
    date -u +"%Y%m%d%H%M%S"
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

# just used in main or entropy-master branch
__elpa_worktrees_init ()
{
    cd "${EemacsextMake_elpadir}"
    make setup
    error_msg "Setup admin worktree fatal"
    make worktrees
    error_msg "Setup external worktrees fatal"
    echo -e "\e[32melpa worktrees init done!\e[0m"
}

# NOTE: just used in main or entropy-master branch which must followed
# gnu-elpa/main or origin/entropy-master branch.
# ......
# remove all local branches unless the current branch and remove all
# checkouted worktrees and admin makefile
__elpa_worktrees_prune ()
{
    cd "${EemacsextMake_elpadir}"
    local ext_brs_logf=./ext_brs_log.txt
    git branch -l --format='%(refname:short)' | grep -v "(HEAD" > "$ext_brs_logf"
    error_msg "List externals pkgs branch fatal"

    do_msg "prune worktree registries"
    git worktree prune

    local _wname=''
    while IFS= read branch ;
    do
        if [ "$branch" != elpa-admin ] \
               && [ "$branch" != entropy-elpa-admin ] \
               && [ "$(git rev-parse --abbrev-ref HEAD)" != "$branch" ]
        then
            _wname=$(echo $branch | sed 's/.*\///g')
            error_msg "retrive <$branch> dir name fatal as <$_wname>"
            if [ -d packages/"$_wname" ]
            then
                do_msg "remove '$_wname' worktree"
                git worktree remove packages/"$_wname" --force
                error_msg "Remove worktree '%s' with fatal"
            fi
            git branch -D "$branch" --force
            error_msg "Delete Branch '%s' with fatal"
        fi
    done < "$ext_brs_logf"

    if [ ! -z "$(git branch -v | grep 'entropy-elpa-admin')" ] && \
           [ -d admin ]
    then
        do_msg "remove admin worktree"
        git worktree remove admin --force
        error_msg "Delete elpa-admin branch fatal"
    fi
    git branch -D elpa-admin --force
    git branch -D entropy-elpa-admin --force

    do_msg "remove tmp log file"
    rm "$ext_brs_logf"
    error_msg "remove tmp log file <$ext_brs_logf> fatal"

    if [ -h ./GNUmakefile ]
    then
        rm ./GNUmakefile
        error_msg "Remove admin makefile <GNUmakefile> fatal"
    fi

    echo -e "\e[32melpa worktrees prune done\e[0m"
}

__elpa_worktrees_update ()
{
    cd "${EemacsextMake_elpadir}"
    if [ -z "$(git remote -v | grep 'gnu-elpa')" ]
    then
        do_msg "add gnu-elpa remote"
        git remote add gnu-elpa 'https://git.savannah.gnu.org/git/emacs/elpa.git'
        error_msg "Add gnu-elpa mirror fatal"
    fi
    git fetch --all
    error_msg "Sync with gnu-elpa mirror fatal"

    local brname=''
    local oldbrname=''
    for brname in  `git branch -al gnu-elpa/* --format='%(refname:short)'`
    do
        oldbrname=$brname
        brname=`echo $brname | sed 's/gnu-elpa\///g'`
        error_msg "sed fatal for brname '$oldbrname'"
        # create pkg branch
        if [ ! -z "$(git branch -l --format='%(refname:short)' | grep "^$brname")" ]
        then
            git branch -D "$brname" --force
        fi
        git checkout -b "$brname" "$oldbrname"
        error_msg "git checkout $brname with upstream $oldbrname fatal"
    done

    # return to default branch
    git checkout entropy-master
    error_msg "fatal checkout to entropy-master branch"
    echo -e "\e[32mYou can now 'git push --all origin' \
to push all gnu-elpa branches to origin but not forget to \
merge main, master, elpa-admin branch to entropy fork later.\e[0m"
}

# must used before elpa repo inited and after elpa repo just init by
# `git submodule`
EemacsextMake_BuildElpa_Recipes_Or_Init ()
{
    local initp=$1
    echo ""
    echo -e "\e[33m==================================================\e[0m"
    echo -e "\e[32mBuilding elpa recipes ...\e[0m"
    echo -e "\e[33m==================================================\e[0m"
    cd "${EemacsextMake_elpadir}"

    if [ ! "$(git rev-parse --abbrev-ref HEAD)" = main ]
    then
        git branch -D main -f
        git checkout -b main origin/entropy-master
        error_msg "Checkout to main branch fatal"
        git branch -D master -f
    fi

    git submodule update --init
    error_msg "submodule init fatal for entropy-elpa"

    # emacs init
    cd emacs
    if [ ! -z "$(git branch -l --format='%(refname:short)' | grep '^entropy-master')" ]
    then
        git branch -D entropy-master
        error_msg "emacs-repo: delete old entropy-master branch fatal"
    fi
    git checkout -b entropy-master HEAD
    error_msg "emacs-repo: checkout new entropy-emacs branch fatal"
    if [ ! -z "$(git branch -l --format='%(refname:short)' | grep '^master')" ]
    then
        git branch -D master
        error_msg "emacs-repo: delete old master branch fatal"
    fi
    git checkout -b master entropy-master
    error_msg "emacs-repo: checkout new master branch fatal"
    git status

    cd "${EemacsextMake_elpadir}"
    __elpa_worktrees_prune
    __elpa_worktrees_init
    if [ -z $initp ]
    then
        do_msg "building all elpa packages"
        make build-all
    else
        echo -e "\e[32mElpa workspace init done\e[0m"
    fi
}

# NOTE: need elpa repo inited
# ......
# Update entropy elpa fork with all branch update with gnu-elpa and
# checkout entropy-master as current branch.
EemacsextMake_BuildElpa_update ()
{
    EemacsextMake_BuildElpa_clean
    echo ""
    echo -e "\e[33m==================================================\e[0m"
    echo -e "\e[32mUpdating elpa recipes ...\e[0m"
    echo -e "\e[33m==================================================\e[0m"
    cd "${EemacsextMake_elpadir}"
    git submodule deinit --all -f
    error_msg "submodule deinit fatal for entropy-elpa"
    __elpa_worktrees_update
}

# NOTE: need elpa repo inited
# ......
# Clean elpa worktree
# ......
# Clean all local branches except entropy-master branch but recreate
# entropy-emacs as current local branch before branches removing unless
# current branch is =entropy-master= or $1 is not empty.
EemacsextMake_BuildElpa_clean ()
{
    local remove_curbranch_p=$1
    echo ""
    echo -e "\e[33m==================================================\e[0m"
    echo -e "\e[32mClean elpa recipes ...\e[0m"
    echo -e "\e[33m==================================================\e[0m"
    cd "${EemacsextMake_elpadir}"
    if [ ! -e .git ]
    then
        do_msg "Initing elpa submodule"
        cd ..
        git submodule update --init elpa
        error_msg "init elpa submodule fatal"
        cd "${EemacsextMake_elpadir}"
        error_msg "cd to elpa fatal"
    fi

    if [ "$(git rev-parse --abbrev-ref HEAD)" != "entropy-master" ]
    then
        do_msg "checking out entropy-master branch"
        git branch -D entropy-master --force
        git checkout -b entropy-master origin/entropy-master
        error_msg "Create entropy-master branch with fatal"
    elif [ ! -z "$remove_curbranch_p" ]
    then
        git checkout origin/entropy-master
        error_msg "Checkout to top fatal"
        git branch -D entropy-master --force
        git checkout -b entropy-master origin/entropy-master
        error_msg "Create entropy-master branch with fatal"
    fi
    git submodule deinit --all -f
    error_msg "submodule deinit fatal for entropy-elpa"
    if [ -d "archive" ]
    then
        rm -rf "${EemacsextMake_elpadir}"/archive
        error_msg "Remove build archive fatal"
    fi
    if [ -d "archive-devel" ]
    then
        rm -rf "${EemacsextMake_elpadir}"/archive-devel
        error_msg "Remove build archive-devel fatal"
    fi
    if [ -d "packages" ]
    then
        rm -rf "${EemacsextMake_elpadir}"/packages
        error_msg "Remove build packages fatal"
    fi

    __elpa_worktrees_prune
    git branch -D main -f
    git branch -D master -f
    echo -e "\e[32mClean elpa repo done!\e[0m"
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

EemacsextMake_fetch_upstreams_commits ()
{
    local logfile="${EemacsextMake_error_log_host}"/upstream_fetch.log
    [[ -e "$logfile" ]] && rm "$logfile"
    local error_cnt=0
    local upstream_host="${EemacsextMake_upstream_submodules_dir}"
    local pkg=''
    cd "${upstream_host}"
    error_msg "CD to ${upstream_host} failed"

    for pkg in *
    do
        cd "$pkg"
        error_msg "CD to upstream pkg dir <$pkg> failed"
        echo "[$(date_str_get)] ---<$pkg>----------" >> "$logfile"
        do_msg "git fetch new commits for package '$pkg' ..."
        git fetch --all 2>> "$logfile"
        if [[ $? -ne 0 ]]
        then
            echo '----------*error and failed*----------' >> "$logfile"
            let ++error_cnt
        else
            echo '----------*success*----------' >> "$logfile"
        fi
        cd "$upstream_host"
    done

    if [[ $error_cnt -ne 0 ]]
    then
        echo -e "\e[31mThere's \e[33m${error_cnt}\e[0m pkgs fetch news with fatal, \
please view log file '$logfile' for details\e[0m"
        exit 1
    else
        exit 0
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

EemacsextMake_Main_GenReleaseTarball ()
{
    local elpa_pkgs_host="${EemacsextMake_elpadir}"/archive
    local elpa_devel_pkgs_host="${EemacsextMake_elpadir}"/archive-devel
    local melpa_pkgs_host="${EemacsextMake_melpadir}"/packages
    local release_root_host="${EemacsextMake_DIR}"/release
    local release_ver=$(cat "${EemacsextMake_DIR}"/version)
    error_msg "eemacs-ext version flag can not be detected!"

    local release_archive_base_name=entropy-emacs-extensions_build_v"${release_ver}"
    local release_archive_tarball_name="${release_archive_base_name}.tar.xz"

    local release_tmp_dir="${release_root_host}/${release_archive_base_name}"
    if [[ -e "${release_tmp_dir}" ]]
    then
        rm -rf "${release_tmp_dir}"
        error_msg "rmdir: <${release_tmp_dir}> with fatal"
    fi
    mkdir -p "${release_tmp_dir}"
    error_msg "makdir: <${release_tmp_dir}> with fatal"

    echo -e "--> cp elpa ..."
    cp -a "${elpa_pkgs_host}" "${release_tmp_dir}"/elpa
    error_msg "cp elpa fatal"
    echo -e "--> cp elpa-devel ..."
    cp -a "${elpa_devel_pkgs_host}" "${release_tmp_dir}"/elpa-devel
    error_msg "cp elpa-devel fatal"
    echo -e "--> cp melpa ..."
    cp -a "${melpa_pkgs_host}" "${release_tmp_dir}"/melpa
    error_msg "cp melpa fatal"

    cd "${release_tmp_dir}"
    error_msg "chdir: <${release_tmp_dir}> fatal"
    echo -e "Gen sha256sum ..."
    # use find to output sha256sum to updir since 'find -type f' will include the output file
    find -type f | xargs sha256sum -b > ../sha256sum.log
    error_msg "sha256sum: for <${release_tmp_dir}> fatal"
    mv ../sha256sum.log .
    error_msg "sha256sum: mv ../sha256sum.log to <${release_tmp_dir}> fatal"
    sha256sum -c ./sha256sum.log > /dev/null
    error_msg "fatal for recheck sha256sum for ${release_archive_base_name}"

    cd "${release_root_host}"
    error_msg "chdir: <${release_root_host}> fatal"

    echo -e "--> make release tarball of ${release_archive_base_name}.tar.xz ..."
    if [[ -e "${release_archive_tarball_name}" ]]
    then
        rm "${release_archive_tarball_name}"
        error_msg "remove old release tarball fatal"
    fi
    tar -Jcf "${release_archive_tarball_name}" "${release_archive_base_name}"
    error_msg  "make release tarball of ${release_archive_tarball_name} fatal"
    echo -e "done"
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
    EemacsextMake_BuildElpa_Recipes_Or_Init
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
    echo -e "- 'release':             generate packages release tarball"
    echo -e ""
    echo -e "--------------------------------maintainability------------------------------------"
    echo -e "- 'sb-upsuggest':        get submodule update suggestions (for *maintainer* only)"
    echo -e "- 'fetch-new':           fetch new commits for melpa upstream packags"
    echo -e "- 'init-elpa'            init elpa workspace"
    echo -e "- 'update-elpa'          update elpa with gnu-elpa"
    echo -e "- 'clean-elpa'           clean elpa worktree and builds (need to have elpa inited)"
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
                            EemacsextMake_BuildElpa_Recipes_Or_Init ;;
        build-eemacs_recipes)
            echo -e "\e[31mOff-line, all of eemacs packages have been migrated into eemacs self.\e[0m"
            # EemacsextMake_Main_Tidyup_WorkTree elements/submodules/eemacs-packages
            # EemacsextMake_BuildRecipes eemacs
            ;;

        clean) cd "$EemacsextMake_DIR" && git clean -xfd . && git submodule deinit --all -f ;;

        all) EemacsextMake_Main_All ;;
        release) EemacsextMake_Main_GenReleaseTarball ;;
        # maintainability part
        sb-upsuggest) EemacsextMake_Main_Choice init
                      EemacsextMake_get_submodule_update_suggestion ;;

        fetch-new)
            EemacsextMake_Main_Choice init
            EemacsextMake_fetch_upstreams_commits
            ;;

        init-elpa) EemacsextMake_Main_Tidyup_WorkTree "$(EemacsextMake_GetRepoPath ${EemacsextMake_elpadir})"
                   EemacsextMake_BuildElpa_Recipes_Or_Init 'init';;
        update-elpa)
            EemacsextMake_Main_Tidyup_WorkTree "$(EemacsextMake_GetRepoPath ${EemacsextMake_elpadir})"
            EemacsextMake_BuildElpa_update ;;
        clean-elpa)
            #EemacsextMake_Main_Tidyup_WorkTree "$(EemacsextMake_GetRepoPath ${EemacsextMake_elpadir})"
            EemacsextMake_BuildElpa_clean ;;

        # Otherwise
        *) EemacsextMake_Main_Help ;;
    esac
}

# * provide

EemacsextMake_Checking_shell

cd "${EemacsextMake_DIR}"

EemacsextMake_MainCommand="$1"
EemacsextMake_Main_Choice "$1"
