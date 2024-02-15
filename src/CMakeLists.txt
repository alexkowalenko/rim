#
# CMakeLists.txt libraries
#

#set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/src)

add_library(rimcommon STATIC
    parameters.f90
    adddat.f
    attadd.f attdel.f attget.f attnew.f attpag.f attput.f
    blkchg.f blkcln.f blkclr.f blkdef.f blkdsp.f blkdwn.f blkext.f
    blkloc.f blkmov.f blknxt.f blkup.f
    btadd.f btget.f btinit.f btlki.f btlkr.f btmove.f btput.f btrep.f btsert.f
    change.f chkatt.f chkrel.f
    deldat.f delete.f
    eq.f
    getdat.f getkca.f gett.f gtsort.f
    isect.f
    join.f kmpard.f kmpari.f kmparr.f kmpart.f kompxx.f
    lnkadd.f lnkget.f lnkpag.f
    locatt.f locboo.f loclnk.f locprm.f locrel.f locvar.f
    motscn.f ne.f 
    parse.f parval.f prckey.f prjtup.f
    putdat.f putt.f ptrs.f 
    reladd.f relget.f relpag.f relput.f
    rmlook.f rmpii.f rmres.f rmsav.f rmvarc.f rtoa.f
    selrel.f selrpt.f selsrt.f selwhr.f
    sort.f subtrc.f swcon.f swcost.f
    swfilo.f swflfs.f 
    swhart.f swhrtd.f swhrti.f swhrtr.f 
    swicst.f swidcp.f swiicp.f swinpo.f swircp.f switcp.f 
    swshel.f swsink.f swsmfl.f swsmvl.f 
    swunlo.f swunvl.f 
    swvacp.f swvlfs.f swvllo.f 
    union.f varadd.f 
    whetol.f wheval.f whrnam.f
    cards.f90
    datetime.f90
    extern.f90
    files.f90
    formater.f90
    globals.f90
    macros.f90
    maths.f90
    message.f90
    lexer.f90
    parser.f90
    randomfiles.f90
    rim.f90
    system.f90
    text.f90
    textfiles.f90
    utils.f90
)

add_library(rimextlib STATIC
    amsg.f 
    lnkput.f loadmv.f msg.f
    pgbres.f pgbsto.f pgcexp.f
    pgcomp.f pgcprt.f pgeexp.f pgeprt.f pgexec.f pgstor.f
    selatt.f selout.f selput.f sgtlnk.f 
    undata.f undef.f unpass.f
    extern_out.f90
    rim_cmd.f90
    system.f90
)

add_library(rimlib STATIC
    pilib.f pilibx.f
    rmxdel.f rmxget.f rmxlod.f rmxput.f
    extern_mute.f90
    system.f90
 )