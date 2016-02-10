# -*- coding: utf-8 -*-

"""
***************************************************************************
    ProcessingPlugin.py
    ---------------------
    Date                 : August 2012
    Copyright            : (C) 2012 by Victor Olaya
    Email                : volayaf at gmail dot com
***************************************************************************
*                                                                         *
*   This program is free software; you can redistribute it and/or modify  *
*   it under the terms of the GNU General Public License as published by  *
*   the Free Software Foundation; either version 2 of the License, or     *
*   (at your option) any later version.                                   *
*                                                                         *
***************************************************************************
"""

from processing import interface

__author__ = 'Victor Olaya'
__date__ = 'August 2012'
__copyright__ = '(C) 2012, Victor Olaya'

# This will get replaced with a git SHA1 when you do a git archive

__revision__ = '$Format:%H$'

import shutil
import os, sys
import inspect
import webbrowser
import csv
from PyQt4 import QtGui, QtCore
from PyQt4.QtCore import *
from PyQt4.QtGui import *
from qgis.core import *
from qgis.gui import *
from processing.core.Processing import Processing
from processing.core.ProcessingConfig import ProcessingConfig
from processing.gui.ProcessingToolbox import ProcessingToolbox
from processing.core.ProcessingResults import ProcessingResults
from processing.gui.HistoryDialog import HistoryDialog
from processing.gui.ConfigDialog import ConfigDialog
from processing.gui.ResultsDialog import ResultsDialog
from processing.gui.StatusDatabase import StatusDatabase
from processing.gui.NumberCombine import NumberCombine
from processing.gui.EditCrossTabulation import EditCrossTabulation
from processing.gui.BatchProcessingDialog import BatchProcessingDialog
from processing.modeler.ModelerDialog import ModelerDialog
from processing.commander.CommanderWindow import CommanderWindow
from processing.algs.Characterize import Characterize
from processing.algs.ftools.Reconcile import Reconcile
from processing.algs.ftools.FixedDistanceBuffer import FixedDistanceBuffer
from processing.algs.ftools.VariableDistanceBuffer import \
        VariableDistanceBuffer
from processing.algs.ftools.ReprojectLayer import ReprojectLayer
from processing.algs.ZonalStatistics import ZonalStatistics
from processing.gdal.ClipByMask import ClipByMask
from processing.gdal.warp import warp
from processing.gui.ParametersDialog import ParametersDialog
from processing.modeler import ModelerAlgorithmProvider
from processing.modeler.ModelerAlgorithm import ModelerAlgorithm
from processing.modeler.Providers import Providers
from processing.molusce.moluscedialog import MolusceDialog
from processing.tablemanager.tableManager_gui import TableManager
from processing.reclasstable.reclasstable_gui import ReclassTable
from processing.GroupStats.GroupStatsDialog import GroupStatsDialog
#from processing.raster_transparency.rastertransparencydockwidget import RasterTransparencyDockWidget
from processing.DockableMirrorMap.dockableMirrorMap import DockableMirrorMap
from processing.r import RAlgorithmProvider
from processing.r.RUtils import RUtils
from processing.r.RAlgorithm import RAlgorithm
from processing.tools import dataobjects
from processing.tools.system import *
import processing.resources_rc

cmd_folder = os.path.split(inspect.getfile(inspect.currentframe()))[0]
if cmd_folder not in sys.path:
    sys.path.insert(0, cmd_folder)


class ProcessingPlugin:

    def __init__(self, iface):
        interface.iface = iface
        Processing.initialize()
        self.canvas = interface.iface.mapCanvas()
        
    def initGui(self):
        self.commander = None
        self.toolbox = ProcessingToolbox()
        self.status = StatusDatabase()
        #self.rasterSlider = RasterTransparencyDockWidget(interface.iface)
        self.mirrorMap = DockableMirrorMap(interface.iface.mainWindow(), interface.iface)
        
        #interface.iface.addDockWidget(Qt.LeftDockWidgetArea, self.rasterSlider)
        interface.iface.addDockWidget(Qt.RightDockWidgetArea, self.status)
        interface.iface.addDockWidget(Qt.RightDockWidgetArea, self.toolbox)
        interface.iface.addDockWidget(Qt.RightDockWidgetArea, self.mirrorMap)
        
        #self.rasterSlider.hide()
        self.status.show()
        self.toolbox.hide()
        self.mirrorMap.hide()
        #Processing.addAlgListListener(self.toolbox)

        #interface.iface.currentLayerChanged.connect(self.layerChanged)
        #self.layerChanged()

        #READ LUMENS LOG FILE
        self.projectIsLoaded = False
        self.activeProject = interface.iface.mainWindow().addToolBar('Active LUMENS Project')
        self.readLUMENSLogFile()

        self.menu = QMenu(interface.iface.mainWindow())
        self.menu.setTitle(QCoreApplication.translate('Processing',
                           'Processing'))
        
        #DATABASE MENU                                          
        self.mDatabaseMenu = QMenu(interface.iface.mainWindow())
        self.mDatabaseMenu.setTitle(QCoreApplication.translate("Database", "Database")) 

        self.mCreateLDBMenu = QAction(QIcon(":/processing/images/lumens.png"),
             QCoreApplication.translate("Database", "Create LUMENS database"),
             interface.iface.mainWindow())
        self.mCreateLDBMenu.triggered.connect(self.openCreateLDB)
        self.mDatabaseMenu.addAction(self.mCreateLDBMenu)
        
        self.mOpenLDBMenu = QAction(QIcon(":/processing/images/lumens.png"),
             QCoreApplication.translate("Database", "Open LUMENS database"),
             interface.iface.mainWindow())
        self.mOpenLDBMenu.triggered.connect(self.openOpenLDB)
        self.mDatabaseMenu.addAction(self.mOpenLDBMenu)

        self.mCloseLDBMenu = QAction(QIcon(":/processing/images/lumens.png"),
             QCoreApplication.translate("Database", "Close LUMENS database"),
             interface.iface.mainWindow())
        self.mCloseLDBMenu.triggered.connect(self.openCloseLDB)
        self.mDatabaseMenu.addAction(self.mCloseLDBMenu)

        self.mAddDataMenu = QMenu(QCoreApplication.translate("Database", "Add data into LUMENS Database"))
        self.mDatabaseMenu.addMenu(self.mAddDataMenu)

        self.mRasterLanduseLDBMenu = QAction(QIcon(":/processing/images/lumens.png"),
             QCoreApplication.translate("Database", "Add land use/cover data"),
             interface.iface.mainWindow())
        self.mRasterLanduseLDBMenu.triggered.connect(self.openRasterLanduseLDB)
        self.mAddDataMenu.addAction(self.mRasterLanduseLDBMenu)
        
        self.mRasterPeatLDBMenu = QAction(QIcon(":/processing/images/lumens.png"),
             QCoreApplication.translate("Database", "Add peat data"),
             interface.iface.mainWindow())
        self.mRasterPeatLDBMenu.triggered.connect(self.openRasterPeatLDB)
        self.mAddDataMenu.addAction(self.mRasterPeatLDBMenu)
        
        self.mRasterFactorLDBMenu = QAction(QIcon(":/processing/images/lumens.png"),
             QCoreApplication.translate("Database", "Add factor data"),
             interface.iface.mainWindow())
        self.mRasterFactorLDBMenu.triggered.connect(self.openRasterFactorLDB)
        self.mAddDataMenu.addAction(self.mRasterFactorLDBMenu)
        
        self.mRasterPlanningUnitLDBMenu = QAction(QIcon(":/processing/images/lumens.png"),
             QCoreApplication.translate("Database", "Add planning unit data"),
             interface.iface.mainWindow())
        self.mRasterPlanningUnitLDBMenu.triggered.connect(self.openRasterPlanningUnitLDB)
        self.mAddDataMenu.addAction(self.mRasterPlanningUnitLDBMenu)

        self.mDelDataMenu = QAction(QIcon(":/processing/images/lumens.png"),
             QCoreApplication.translate("Database", "Delete LUMENS data"),
             interface.iface.mainWindow())
        self.mDelDataMenu.triggered.connect(self.openDelData)
        self.mDatabaseMenu.addAction(self.mDelDataMenu)
        
        self.mImportLDBMenu = QAction(QIcon(":/processing/images/lumens.png"),
             QCoreApplication.translate("Database", "Import LUMENS database"),
             interface.iface.mainWindow())
        self.mImportLDBMenu.triggered.connect(self.openImportLDB)
        self.mDatabaseMenu.addAction(self.mImportLDBMenu)
        #END OF DATABASE MENU
            
        #PUR MENU               
        self.mPurMenu = QMenu(interface.iface.mainWindow())
        self.mPurMenu.setTitle(QCoreApplication.translate("PUR", "PUR")) 

        self.mReconcileByRefClassMenu = QMenu(QCoreApplication.translate("PUR", "Functions-based Reconciliation"))
        self.mPurMenu.addMenu(self.mReconcileByRefClassMenu)

        self.mReconcileStep1Menu = QAction(QIcon(":/processing/images/reconcile.png"),
             QCoreApplication.translate("PUR", "Select reference data and prepare planning unit"),
             interface.iface.mainWindow())
        self.mReconcileStep1Menu.triggered.connect(self.openReconcileStep1)
        self.mReconcileByRefClassMenu.addAction(self.mReconcileStep1Menu)

        #=======================================================================
        # self.mReconcileStep2Menu = QAction(QIcon(":/processing/images/reconcile.png"),
        #      QCoreApplication.translate("PUR", "Select planning units"),
        #      interface.iface.mainWindow())
        # self.mReconcileStep2Menu.triggered.connect(self.openReconcileStep2)
        # self.mReconcileByRefClassMenu.addAction(self.mReconcileStep2Menu)
        #=======================================================================

        self.mReconcileStep3Menu = QAction(QIcon(":/processing/images/reconcile.png"),
             QCoreApplication.translate("PUR", "Conduct reconciliation"),
             interface.iface.mainWindow())
        self.mReconcileStep3Menu.triggered.connect(self.openReconcileStep3)
        self.mReconcileByRefClassMenu.addAction(self.mReconcileStep3Menu)

        #=======================================================================
        # self.mReconcileStep4Menu = QAction(QIcon(":/processing/images/reconcile.png"),
        #      QCoreApplication.translate("PUR", "Finalize planning unit"),
        #      interface.iface.mainWindow())
        # self.mReconcileStep4Menu.triggered.connect(self.openReconcileStep4)
        # self.mReconcileByRefClassMenu.addAction(self.mReconcileStep4Menu)
        #=======================================================================

        self.mCombinePUMenu = QAction(QIcon(":/processing/images/reconcile.png"),
             QCoreApplication.translate("PUR", "Combine planning unit"),
             interface.iface.mainWindow())
        self.mCombinePUMenu.triggered.connect(self.openCombinePU)
        self.mPurMenu.addAction(self.mCombinePUMenu)

        self.mReconcileByAHPMenu = QAction(QIcon(":/processing/images/reconcile.png"),
             QCoreApplication.translate("PUR", "Reconcile using AHP"),
             interface.iface.mainWindow())
        self.mReconcileByAHPMenu.triggered.connect(self.openReconcile)
        self.mPurMenu.addAction(self.mReconcileByAHPMenu)
 
 #==============================================================================
 #        self.mReconcileAppend = QAction(QIcon(":/processing/images/reconcile.png"),
 #             QCoreApplication.translate("PUR", "Add special planning unit"),
 #             interface.iface.mainWindow())
 #        self.mReconcileAppend.triggered.connect(self.openAppend)
 #        self.mPurMenu.addAction(self.mReconcileAppend)
 #         
 # 
 #        self.mPriorityTableMenu = QAction(QIcon(":/processing/images/reconcile.png"),
 #             QCoreApplication.translate("PUR", "Manual Reconciliation"),
 #             interface.iface.mainWindow())
 #        self.mPriorityTableMenu.triggered.connect(self.openReconcile)
 #        self.mPurMenu.addAction(self.mPriorityTableMenu)        
 #                    
 #        self.mCombineMenu = QAction(QIcon(":/processing/images/build.png"),
 #             QCoreApplication.translate("PUR", "Combine planning unit layers"),
 #             interface.iface.mainWindow())
 #        self.mCombineMenu.triggered.connect(self.openCombineModel)
 #        self.mBuildMenu.addAction(self.mCombineMenu)                    
 #============================================================================== 
        #END OF PUR MENU
                        
        #QUES MENU
        self.mQuesMenu = QMenu(interface.iface.mainWindow())
        self.mQuesMenu.setTitle(QCoreApplication.translate("QUES", "QUES"))
        
        self.mPreQuesMenu = QMenu(QCoreApplication.translate("QUES", "Pre-QUES"))
        self.mQuesMenu.addMenu(self.mPreQuesMenu)
        
        self.mLCCMenu = QAction(QIcon(":/processing/images/pre-ques.png"),
             QCoreApplication.translate("QUES", "Land cover change analysis"),
             interface.iface.mainWindow())
        self.mLCCMenu.triggered.connect(self.openLCC)
        self.mPreQuesMenu.addAction(self.mLCCMenu)
        
        self.mLCTMenu = QAction(QIcon(":/processing/images/pre-ques.png"),
             QCoreApplication.translate("QUES", "Land cover trajectories analysis"),
             interface.iface.mainWindow())
        self.mLCTMenu.triggered.connect(self.openLCT)
        self.mPreQuesMenu.addAction(self.mLCTMenu)
        
        self.mCarbonMenu = QMenu(QCoreApplication.translate("QUES", "QUES-C"))
        self.mQuesMenu.addMenu(self.mCarbonMenu)
        
        self.mCarbonQuesMenu = QAction(QIcon(":/processing/images/carbon.png"),
             QCoreApplication.translate("QUES", "Carbon accounting"),
             interface.iface.mainWindow())
        self.mCarbonQuesMenu.triggered.connect(self.openCarbon)
        self.mCarbonMenu.addAction(self.mCarbonQuesMenu)       
        
        self.mPeatCarbonQuesMenu = QAction(QIcon(":/processing/images/carbon.png"),
             QCoreApplication.translate("QUES", "Peatland carbon accounting"),
             interface.iface.mainWindow())
        self.mPeatCarbonQuesMenu.triggered.connect(self.openPeatCarbon)
        self.mCarbonMenu.addAction(self.mPeatCarbonQuesMenu)       

        self.mSumMultiPeriodMenu = QAction(QIcon(":/processing/images/carbon.png"),
             QCoreApplication.translate("QUES", "Summarize multiple periode"),
             interface.iface.mainWindow())
        self.mSumMultiPeriodMenu.triggered.connect(self.openSumMultiPeriod)
        self.mCarbonMenu.addAction(self.mSumMultiPeriodMenu)      
          
#===============================================================================
#         self.mCarbonInteractiveMenu = QAction(QIcon(":/processing/images/carbon.png"),
#              QCoreApplication.translate("QUES", "Interactive carbon accounting"),
#              interface.iface.mainWindow())
#         self.mCarbonInteractiveMenu.triggered.connect(self.openCarbonInteractive)
#         self.mCarbonMenu.addAction(self.mCarbonInteractiveMenu)   
# 
#         self.mCarbonStepMenu = QMenu(QCoreApplication.translate("QUES", "Step by step"))
#         self.mCarbonMenu.addMenu(self.mCarbonStepMenu)
#  
#         self.mCarbonStep1Menu = QAction(QIcon(":/processing/images/carbon.png"),
#              QCoreApplication.translate("QUES", "Create carbon density maps"),
#              interface.iface.mainWindow())
#         self.mCarbonQuesMenu.triggered.connect(self.openCarbon)
#         self.mCarbonStepMenu.addAction(self.mCarbonStep1Menu)        
#          
#         self.mCarbonStep2Menu = QAction(QIcon(":/processing/images/carbon.png"),
#              QCoreApplication.translate("QUES", "Calculate emission/sequestration"),
#              interface.iface.mainWindow())
#         self.mCarbonQuesMenu.triggered.connect(self.openCarbon)
#         self.mCarbonStepMenu.addAction(self.mCarbonStep2Menu)        
#          
#         self.mCarbonStep3Menu = QAction(QIcon(":/processing/images/carbon.png"),
#              QCoreApplication.translate("QUES", "Create emission/sequestration maps"),
#              interface.iface.mainWindow())
#         self.mCarbonQuesMenu.triggered.connect(self.openCarbon)
#         self.mCarbonStepMenu.addAction(self.mCarbonStep3Menu)      
#          
#         self.mCarbonStep4Menu = QAction(QIcon(":/processing/images/carbon.png"),
#              QCoreApplication.translate("QUES", "Build QUES-C database"),
#              interface.iface.mainWindow())
#         self.mCarbonQuesMenu.triggered.connect(self.openCarbon)
#         self.mCarbonStepMenu.addAction(self.mCarbonStep4Menu)
#              
#         self.mCarbonStep5Menu = QAction(QIcon(":/processing/images/carbon.png"),
#              QCoreApplication.translate("QUES", "Build Output"),
#              interface.iface.mainWindow())
#         self.mCarbonQuesMenu.triggered.connect(self.openCarbon)
#         self.mCarbonStepMenu.addAction(self.mCarbonStep5Menu)
#===============================================================================
                      
        self.mBiodiversityMenu = QMenu(QCoreApplication.translate("QUES", "QUES-B"))
        self.mQuesMenu.addMenu(self.mBiodiversityMenu)
 
        self.mBiodiversityQuesMenu = QAction(QIcon(":/processing/images/biodiversity.png"),
             QCoreApplication.translate("QUES", "QUES-B Analysis"),
             interface.iface.mainWindow())
        self.mBiodiversityQuesMenu.triggered.connect(self.openBiodiversity)
        self.mBiodiversityMenu.addAction(self.mBiodiversityQuesMenu)
        
        self.mHydroMenu = QMenu(QCoreApplication.translate("QUES", "QUES-H"))
        self.mQuesMenu.addMenu(self.mHydroMenu)

   #============================================================================
   #      self.mWatershedAnalysisMenu = QAction(QIcon(":/processing/images/hydro.png"),
   #           QCoreApplication.translate("QUES", "Generate watershed boundary"),
   #           interface.iface.mainWindow())
   #      self.mWatershedAnalysisMenu.triggered.connect(self.openWatershedAnalysisModel)
   #      self.mHydroMenu.addAction(self.mWatershedAnalysisMenu)  
   # 
   #      self.mDefCatchmentMenu = QAction(QIcon(":/processing/images/hydro.png"),
   #           QCoreApplication.translate("QUES", "Define Catchment Area"),
   #           interface.iface.mainWindow())
   #      self.mDefCatchmentMenu.triggered.connect(self.openDefCatchment)
   #      self.mHydroMenu.addAction(self.mDefCatchmentMenu)  
   # 
   #      self.mLCFracMenu = QAction(QIcon(":/processing/images/hydro.png"),
   #           QCoreApplication.translate("QUES", "Land cover fraction"),
   #           interface.iface.mainWindow())
   #      self.mLCFracMenu.triggered.connect(self.openLCFrac)
   #      self.mHydroMenu.addAction(self.mLCFracMenu)  
   # 
   #      self.mHydroModelMenu = QAction(QIcon(":/processing/images/hydro.png"),
   #           QCoreApplication.translate("QUES", "Hydrological modeling"),
   #           interface.iface.mainWindow())
   #      self.mHydroModelMenu.triggered.connect(self.openHydroModel)
   #      self.mHydroMenu.addAction(self.mHydroModelMenu)      
   #============================================================================

        self.mWatershedDelMenu = QAction(QIcon(":/processing/images/hydro.png"),
             QCoreApplication.translate("QUES", "Watershed delineation"),
             interface.iface.mainWindow())
        self.mWatershedDelMenu.triggered.connect(self.openWatershedDel)
        self.mHydroMenu.addAction(self.mWatershedDelMenu) 
    
        self.mHRUDefMenu = QMenu(QCoreApplication.translate("QUES", "Hydrological Response Unit Definition"))
        self.mHydroMenu.addMenu(self.mHRUDefMenu)  

        self.mDominantHRUMenu = QAction(QIcon(":/processing/images/hydro.png"),
             QCoreApplication.translate("QUES", "Dominant HRU"),
             interface.iface.mainWindow()) 
        self.mDominantHRUMenu.triggered.connect(self.openDominantHRU)
        self.mHRUDefMenu.addAction(self.mDominantHRUMenu)    

        self.mDominantLUSSLMenu = QAction(QIcon(":/processing/images/hydro.png"),
             QCoreApplication.translate("QUES", "Dominant Land Use, Soil, and Slope"),
             interface.iface.mainWindow()) 
        self.mDominantLUSSLMenu.triggered.connect(self.openDominantLUSSL)
        self.mHRUDefMenu.addAction(self.mDominantLUSSLMenu)   
        
        self.mMultipleHRUMenu = QAction(QIcon(":/processing/images/hydro.png"),
             QCoreApplication.translate("QUES", "Multiple HRU"),
             interface.iface.mainWindow()) 
        self.mMultipleHRUMenu.triggered.connect(self.openMultipleHRU)
        self.mHRUDefMenu.addAction(self.mMultipleHRUMenu)   
        
        self.mWatershedModEvalMenu = QAction(QIcon(":/processing/images/hydro.png"),
             QCoreApplication.translate("QUES", "Watershed model evaluation"),
             interface.iface.mainWindow())
        self.mWatershedModEvalMenu.triggered.connect(self.openWatershedModEval)
        self.mHydroMenu.addAction(self.mWatershedModEvalMenu) 
 
        self.mWatershedIndMenu = QAction(QIcon(":/processing/images/hydro.png"),
             QCoreApplication.translate("QUES", "Watershed indicators"),
             interface.iface.mainWindow())
        self.mWatershedIndMenu.triggered.connect(self.openWatershedInd)
        self.mHydroMenu.addAction(self.mWatershedIndMenu)

        '''    
        self.mBiodiversityStepMenu = QMenu(QCoreApplication.translate("QUES", "Step by step"))
        self.mBiodiversityMenu.addMenu(self.mBiodiversityStepMenu)    
        
        self.mPosQuesMenu = QAction(QIcon(":/processing/images/pos-ques.png"),
             QCoreApplication.translate("QUES", "P&os-QUES"),
             interface.iface.mainWindow())
        self.resultsAction.triggered.connect(self.openResults)
        self.mQuesMenu.addAction(self.mPosQuesMenu)
        '''
        #END OF QUES
        
        #TA
        self.mTaMenu = QMenu(interface.iface.mainWindow())
        self.mTaMenu.setTitle(QCoreApplication.translate("TA", "TA"))
        
        self.mOpcostSubMenu = QMenu(QCoreApplication.translate("TA", "Opportunity cost"))
        self.mTaMenu.addMenu(self.mOpcostSubMenu)  
        
        self.mAbacusOpCostMenu = QAction(QIcon(":/processing/images/ta.png"),
             QCoreApplication.translate("TA", "Abacus opportunity cost curve"),
             interface.iface.mainWindow())
        self.mAbacusOpCostMenu.triggered.connect(self.openAbacusOpCost)
        self.mOpcostSubMenu.addAction(self.mAbacusOpCostMenu) 
         
        self.mOpCostMenu = QAction(QIcon(":/processing/images/ta.png"),
             QCoreApplication.translate("TA", "Opportunity cost curve"),
             interface.iface.mainWindow())
        self.mOpCostMenu.triggered.connect(self.openOpCost)
        self.mOpcostSubMenu.addAction(self.mOpCostMenu) 
        
        self.mOpCostMenu = QAction(QIcon(":/processing/images/ta.png"),
             QCoreApplication.translate("TA", "Opportunity cost map"),
             interface.iface.mainWindow())
        self.mOpCostMenu.triggered.connect(self.openOpCost2)
        self.mOpcostSubMenu.addAction(self.mOpCostMenu)   

        self.mRegEconomyMenu = QMenu(QCoreApplication.translate("TA", "Regional Economy"))
        self.mTaMenu.addMenu(self.mRegEconomyMenu)  
          
        self.mSingleIODAMenu = QAction(QIcon(":/processing/images/ta.png"),
             QCoreApplication.translate("TA", "Descriptive analysis of regional economy  (Single time series)"),
             interface.iface.mainWindow())
        self.mSingleIODAMenu.triggered.connect(self.openSingleIODA)
        self.mRegEconomyMenu.addAction(self.mSingleIODAMenu)  
        
        self.mTimeSeriesIOMenu = QAction(QIcon(":/processing/images/ta.png"),
             QCoreApplication.translate("TA", "Descriptive analysis of regional economy (Multiple time series)"),
             interface.iface.mainWindow())
        self.mTimeSeriesIOMenu.triggered.connect(self.openTimeSeriesIO)
        self.mRegEconomyMenu.addAction(self.mTimeSeriesIOMenu)
        
        self.mLandDistLRMenu = QAction(QIcon(":/processing/images/ta.png"),
             QCoreApplication.translate("TA", "Land Requirement Analysis"),
             interface.iface.mainWindow())
        self.mLandDistLRMenu.triggered.connect(self.openLandDistLR)
        self.mRegEconomyMenu.addAction(self.mLandDistLRMenu)  
        
        self.mImpactLUMenu = QAction(QIcon(":/processing/images/ta.png"),
             QCoreApplication.translate("TA", "Impact of land use change to regional economy"),
             interface.iface.mainWindow())
        self.mImpactLUMenu.triggered.connect(self.openImpactLU)
        self.mRegEconomyMenu.addAction(self.mImpactLUMenu)  
        
        self.mFinalDemandMenu = QAction(QIcon(":/processing/images/ta.png"),
             QCoreApplication.translate("TA", "Impact of regional economic scenario to land use change (Final Demand Scenario)"),
             interface.iface.mainWindow())
        self.mFinalDemandMenu.triggered.connect(self.openFinalDemand)
        self.mRegEconomyMenu.addAction(self.mFinalDemandMenu)  
        
        self.mGDPMenu = QAction(QIcon(":/processing/images/ta.png"),
             QCoreApplication.translate("TA", "Impact of regional economic scenario to land use change (GDP Scenario)"),
             interface.iface.mainWindow())
        self.mGDPMenu.triggered.connect(self.openGDP)
        self.mRegEconomyMenu.addAction(self.mGDPMenu)  
        #END OF TA
        
        #SCIENDO
        self.mSciendoMenu = QMenu(interface.iface.mainWindow())
        self.mSciendoMenu.setTitle(QCoreApplication.translate("SCIENDO", "SCIENDO"))

        self.mLEDSMenu = QMenu(QCoreApplication.translate("SCIENDO", "Low emission development analysis"))
        self.mSciendoMenu.addMenu(self.mLEDSMenu)  

        self.mHistBaselineMenu = QMenu(QCoreApplication.translate("SCIENDO", "Historical baseline"))
        self.mLEDSMenu.addMenu(self.mHistBaselineMenu) 

        self.mHistBaselineAnnMenu = QAction(QIcon(":/processing/images/sciendo.png"),
             QCoreApplication.translate("SCIENDO", "Annual Projection"),
             interface.iface.mainWindow()) 
        self.mHistBaselineAnnMenu.triggered.connect(self.openHistAnnualProj)
        self.mHistBaselineMenu.addAction(self.mHistBaselineAnnMenu)

        self.mBasedPeriodeMenu = QAction(QIcon(":/processing/images/sciendo.png"),
             QCoreApplication.translate("SCIENDO", "Based on period"),
             interface.iface.mainWindow()) 
        self.mBasedPeriodeMenu.triggered.connect(self.openHistBaselineGUI)
        self.mHistBaselineMenu.addAction(self.mBasedPeriodeMenu)
        
        self.mDriversAnalysisMenu = QAction(QIcon(":/processing/images/sciendo.png"),
             QCoreApplication.translate("SCIENDO", "Drivers analysis"),
             interface.iface.mainWindow()) 
        self.mDriversAnalysisMenu.triggered.connect(self.openDriversAnalysis)
        self.mLEDSMenu.addAction(self.mDriversAnalysisMenu)

        #=======================================================================
        # self.mScenarioMenu = QAction(QIcon(":/processing/images/sciendo.png"),
        #      QCoreApplication.translate("SCIENDO", "Projection on scenario"),
        #      interface.iface.mainWindow()) 
        # self.mScenarioMenu.triggered.connect(self.openScenario)
        # self.mLEDSMenu.addAction(self.mScenarioMenu)
        #        
        # self.mCalcEmMenu = QAction(QIcon(":/processing/images/sciendo.png"),
        #      QCoreApplication.translate("SCIENDO", "Calculate emission reduction"),
        #      interface.iface.mainWindow()) 
        # self.mCalcEmMenu.triggered.connect(self.openCalcEmissionReduction)
        # self.mLEDSMenu.addAction(self.mCalcEmMenu)
        #=======================================================================
        
   #============================================================================
   #      self.mCalcEmMultScenMenu = QAction(QIcon(":/processing/images/sciendo.png"),
   #           QCoreApplication.translate("SCIENDO", "Compare scenario"),
   #           interface.iface.mainWindow()) 
   #      self.mCalcEmMultScenMenu.triggered.connect(self.openCompareScenario)
   #      self.mLEDSMenu.addAction(self.mCalcEmMultScenMenu)
   # 
   #      self.mBuildScenarioMenu = QMenu(QCoreApplication.translate("SCIENDO", "Build scenario"))
   #      self.mLEDSMenu.addMenu(self.mBuildScenarioMenu)  
   #      
   #      self.mAbacusScenarioMenu = QAction(QIcon(":/processing/images/sciendo.png"),
   #           QCoreApplication.translate("SCIENDO", "Using proportion of area"),
   #           interface.iface.mainWindow()) 
   #      self.mAbacusScenarioMenu.triggered.connect(self.openAbacusScenario)
   #      self.mBuildScenarioMenu.addAction(self.mAbacusScenarioMenu)
   #============================================================================
        
        self.mAbacusAbsAreaMenu = QAction(QIcon(":/processing/images/sciendo.png"),
             QCoreApplication.translate("SCIENDO", "Build scenario"),
             interface.iface.mainWindow()) 
        self.mAbacusAbsAreaMenu.triggered.connect(self.openAbacusAbsArea)
        self.mLEDSMenu.addAction(self.mAbacusAbsAreaMenu)        

        self.mLUCModelingMenu = QMenu(QCoreApplication.translate("SCIENDO", "Land use change modeling"))
        self.mSciendoMenu.addMenu(self.mLUCModelingMenu)  

        '''DINAMICA by AE'''
        self.mDataPrepPhaseMenu = QAction(QIcon(":/processing/images/sciendo.png"),
             QCoreApplication.translate("SCIENDO", "Calculate transition matrix"),
             interface.iface.mainWindow()) 
        self.mDataPrepPhaseMenu.triggered.connect(self.openSCIENDOTCM)
        self.mLUCModelingMenu.addAction(self.mDataPrepPhaseMenu)
        
        self.mCreateSamplesPhaseMenu = QAction(QIcon(":/processing/images/sciendo.png"),
             QCoreApplication.translate("SCIENDO", "Create raster cube of factors"),
             interface.iface.mainWindow()) 
        self.mCreateSamplesPhaseMenu.triggered.connect(self.openSCIENDORC)
        self.mLUCModelingMenu.addAction(self.mCreateSamplesPhaseMenu)

        self.mTrainingMLPPhaseMenu = QAction(QIcon(":/processing/images/sciendo.png"),
             QCoreApplication.translate("SCIENDO", "Calculate weight of evidence"),
             interface.iface.mainWindow()) 
        self.mTrainingMLPPhaseMenu.triggered.connect(self.openSCIENDOWOE)
        self.mLUCModelingMenu.addAction(self.mTrainingMLPPhaseMenu)
        
        self.mPredictionPhaseMenu = QAction(QIcon(":/processing/images/sciendo.png"),
             QCoreApplication.translate("SCIENDO", "Simulate land use change"),
             interface.iface.mainWindow()) 
        self.mPredictionPhaseMenu.triggered.connect(self.openSCIENDOSLU)
        self.mLUCModelingMenu.addAction(self.mPredictionPhaseMenu)


        self.mPredictionPhase2Menu = QAction(QIcon(":/processing/images/sciendo.png"),
             QCoreApplication.translate("SCIENDO", "Simulate with scenario"),
             interface.iface.mainWindow()) 
        self.mPredictionPhase2Menu.triggered.connect(self.openSCIENDOSLU2)
        self.mLUCModelingMenu.addAction(self.mPredictionPhase2Menu)


        '''LTM by Alfa
        self.mDataPrepPhaseMenu = QAction(QIcon(":/processing/images/sciendo.png"),
             QCoreApplication.translate("SCIENDO", "Data preparation phase"),
             interface.iface.mainWindow()) 
        #self.mDataPrepPhaseMenu.triggered.connect(self.openHistBaseline)
        self.mLUCModelingMenu.addAction(self.mDataPrepPhaseMenu)
        
        self.mCreateSamplesPhaseMenu = QAction(QIcon(":/processing/images/sciendo.png"),
             QCoreApplication.translate("SCIENDO", "Creating samples phase"),
             interface.iface.mainWindow()) 
        #self.mCreateSamplesPhaseMenu.triggered.connect(self.openScenario)
        self.mLUCModelingMenu.addAction(self.mCreateSamplesPhaseMenu)

        self.mTrainingMLPPhaseMenu = QAction(QIcon(":/processing/images/sciendo.png"),
             QCoreApplication.translate("SCIENDO", "Training Multi-Layer Perceptron phase"),
             interface.iface.mainWindow()) 
        #self.mTrainingMLPPhaseMenu.triggered.connect(self.openScenario)
        self.mLUCModelingMenu.addAction(self.mTrainingMLPPhaseMenu)
        
        self.mPredictionPhaseMenu = QAction(QIcon(":/processing/images/sciendo.png"),
             QCoreApplication.translate("SCIENDO", "Prediction future land use change phase"),
             interface.iface.mainWindow()) 
        #self.mPredictionPhaseMenu.triggered.connect(self.openScenario)
        self.mLUCModelingMenu.addAction(self.mPredictionPhaseMenu)
        '''
        
        #=======================================================================
        #  self.mLEDSStepMenu = QMenu(QCoreApplication.translate("SCIENDO", "Step by step"))
        #  self.mLEDSMenu.addMenu(self.mLEDSStepMenu)         
        # 
        #  self.mMolusceMenu = QAction(QIcon(":/processing/images/sciendo.png"),
        #       QCoreApplication.translate("SCIENDO", "SCIENDO"),
        #       interface.iface.mainWindow())
        #  self.mMolusceMenu.triggered.connect(self.openMolusce)
        #  self.mSciendoMenu.addAction(self.mMolusceMenu)
        #=======================================================================
        #END OF SCIENDO

        self.mToolsMenu = QMenu(interface.iface.mainWindow())
        self.mToolsMenu.setTitle(QCoreApplication.translate("Tools", "Tools"))

        self.mAbacusMenu = QAction(QIcon(":/processing/images/config.png"),
             QCoreApplication.translate("Tools", "REDD Abacus SP"),
             interface.iface.mainWindow())
        self.mAbacusMenu.triggered.connect(self.openAbacus)
        self.mToolsMenu.addAction(self.mAbacusMenu)  

        self.mLUTMenu = QAction(QIcon(":/processing/images/config.png"),
             QCoreApplication.translate("Tools", "Build lookup table"),
             interface.iface.mainWindow())
        self.mLUTMenu.triggered.connect(self.openReclass)
        self.mToolsMenu.addAction(self.mLUTMenu)        

        self.mCalcHistMenu = QAction(QIcon(":/processing/images/config.png"),
             QCoreApplication.translate("SCIENDO", "Calculate projection historical baseline"),
             interface.iface.mainWindow()) 
        self.mCalcHistMenu.triggered.connect(self.openCalcHist)
        self.mToolsMenu.addAction(self.mCalcHistMenu)

        self.mElevationMenu = QAction(QIcon(":/processing/images/config.png"),
             QCoreApplication.translate("Tools", "Classify elevation"),
             interface.iface.mainWindow())
        self.mElevationMenu.triggered.connect(self.openElevationModel)
        self.mToolsMenu.addAction(self.mElevationMenu)   
        
        self.mClipByMaskMenu = QAction(QIcon(":/processing/images/config.png"),
             QCoreApplication.translate("Tools", "Clip raster by mask layer"),
             interface.iface.mainWindow())
        self.mClipByMaskMenu.triggered.connect(self.openClipByMask)
        self.mToolsMenu.addAction(self.mClipByMaskMenu) 

        self.mToRasterMenu = QAction(QIcon(":/processing/images/config.png"),
             QCoreApplication.translate("Tools", "Convert to raster"),
             interface.iface.mainWindow())
        self.mToRasterMenu.triggered.connect(self.openRasterize)
        self.mToolsMenu.addAction(self.mToRasterMenu)        

        self.mManualMenu = QAction(QIcon(":/processing/images/config.png"),
             QCoreApplication.translate("Tools", "Dissolve"),
             interface.iface.mainWindow())
        self.mManualMenu.triggered.connect(self.openReconcileOld)
        self.mToolsMenu.addAction(self.mManualMenu)
                
        self.mDistanceMenu = QMenu(QCoreApplication.translate("Tools", "Distance analysis"))
        self.mToolsMenu.addMenu(self.mDistanceMenu)
        
        self.mCalculateDistanceMenu = QAction(QIcon(":/processing/images/config.png"),
             QCoreApplication.translate("Tools", "Calculate distance"),
             interface.iface.mainWindow())
        self.mCalculateDistanceMenu.triggered.connect(self.openDistance)
        self.mDistanceMenu.addAction(self.mCalculateDistanceMenu)          

        self.mFixedDistanceBufferMenu = QAction(QIcon(":/processing/images/config.png"),
             QCoreApplication.translate("Tools", "Fixed distance buffer"),
             interface.iface.mainWindow())
        self.mFixedDistanceBufferMenu.triggered.connect(self.openFixedDistanceBuffer)
        self.mDistanceMenu.addAction(self.mFixedDistanceBufferMenu)
        
        self.mVarDistanceBufferMenu = QAction(QIcon(":/processing/images/config.png"),
             QCoreApplication.translate("Tools", "Variable distance buffer"),
             interface.iface.mainWindow())
        self.mVarDistanceBufferMenu.triggered.connect(self.openVariableDistanceBuffer)
        self.mDistanceMenu.addAction(self.mVarDistanceBufferMenu)     

        self.mSlopeMenu = QAction(QIcon(":/processing/images/config.png"),
             QCoreApplication.translate("Tools", "Generate slope"),
             interface.iface.mainWindow())
        self.mSlopeMenu.triggered.connect(self.openSlopeModel)
        self.mToolsMenu.addAction(self.mSlopeMenu)                  

        self.mLCZonalMenu = QAction(QIcon(":/processing/images/config.png"),
             QCoreApplication.translate("Tools", "Land cover zonal statistic"),
             interface.iface.mainWindow())
        self.mLCZonalMenu.triggered.connect(self.openGroupStats)
        self.mToolsMenu.addAction(self.mLCZonalMenu)

        self.mReprojectVectorMenu = QAction(QIcon(":/processing/images/config.png"),
             QCoreApplication.translate("Tools", "Reproject vector"),
             interface.iface.mainWindow())
        self.mReprojectVectorMenu.triggered.connect(self.openReprojectVector)
        self.mToolsMenu.addAction(self.mReprojectVectorMenu) 
        
        self.mReprojectRasterMenu = QAction(QIcon(":/processing/images/config.png"),
             QCoreApplication.translate("Tools", "Reproject raster"),
             interface.iface.mainWindow())
        self.mReprojectRasterMenu.triggered.connect(self.openReprojectRaster)
        self.mToolsMenu.addAction(self.mReprojectRasterMenu)

        self.mStatsMenu = QMenu(QCoreApplication.translate("Tools", "Statistics"))
        self.mToolsMenu.addMenu(self.mStatsMenu)
        
        self.mAreaMenu = QAction(QIcon(":/processing/images/config.png"),
             QCoreApplication.translate("Tools", "Area statistic"),
             interface.iface.mainWindow())
        self.mAreaMenu.triggered.connect(self.openCharacterize)
        self.mStatsMenu.addAction(self.mAreaMenu)

        self.mZonalMenu = QAction(QIcon(":/processing/images/config.png"),
             QCoreApplication.translate("Tools", "Zonal statistic"),
             interface.iface.mainWindow())
        self.mZonalMenu.triggered.connect(self.openZonalStatistics)
        self.mStatsMenu.addAction(self.mZonalMenu)
        
        self.mCrossTabMenu = QAction(QIcon(":/processing/images/config.png"),
             QCoreApplication.translate("Tools", "Cross tabulation"),
             interface.iface.mainWindow())
        self.mCrossTabMenu.triggered.connect(self.openCrossTab)
        self.mStatsMenu.addAction(self.mCrossTabMenu)
        
        self.mEditCrossTabMenu = QAction(QIcon(":/processing/images/config.png"),
             QCoreApplication.translate("Tools", "Edit cross tabulation"),
             interface.iface.mainWindow())
        self.mEditCrossTabMenu.triggered.connect(self.openEditCrossTab)
        self.mStatsMenu.addAction(self.mEditCrossTabMenu)

        #processing
        self.toolboxAction = self.toolbox.toggleViewAction()
        self.toolboxAction.setIcon(QIcon(':/processing/images/alg.png'))
        self.toolboxAction.setText(QCoreApplication.translate('Processing',
                                   'Toolbox'))
        self.menu.addAction(self.toolboxAction)

        '''
        self.rasterAction = self.rasterSlider.toggleViewAction()
        self.rasterAction.setIcon(QIcon(':/processing/images/rasterslider.png'))
        self.rasterAction.setText(QCoreApplication.translate('Processing',
                                   'Raster Slider'))
        self.menu.addAction(self.rasterAction)
        '''
    
        self.mirrorAction = self.mirrorMap.toggleViewAction()
        self.mirrorAction.setIcon(QIcon(':/processing/images/config.png'))
        self.mirrorAction.setText(QCoreApplication.translate('Processing',
                                   'Mirror Map'))
        self.menu.addAction(self.mirrorAction)
        
#         self.statusAction = self.status.toggleViewAction()
#         self.statusAction.setIcon(QIcon(':/processing/images/database.png'))
#         self.statusAction.setText(QCoreApplication.translate('Processing',
#                                    'Status LUMENS Database'))

        self.modelerAction = QAction(QIcon(':/processing/images/model.png'),
                                     QCoreApplication.translate('Processing',
                                     'Graphical modeler'),
                                     interface.iface.mainWindow())
        self.modelerAction.triggered.connect(self.openModeler)
        self.menu.addAction(self.modelerAction)

        self.historyAction = QAction(QIcon(':/processing/images/history.gif'),
                                     QCoreApplication.translate('Processing',
                                     'History and log'),
                                     interface.iface.mainWindow())
        self.historyAction.triggered.connect(self.openHistory)
        self.menu.addAction(self.historyAction)

        self.configAction = QAction(QIcon(':/processing/images/config.png'),
                                    QCoreApplication.translate('Processing',
                                    'Options and configuration'),
                                    interface.iface.mainWindow())
        self.configAction.triggered.connect(self.openConfig)
        self.menu.addAction(self.configAction)

        self.resultsAction = QAction(QIcon(':/processing/images/results.png'),
                                     QCoreApplication.translate('Processing',
                                     '&Results viewer'),
                                     interface.iface.mainWindow())
        self.resultsAction.triggered.connect(self.openResults)
        self.menu.addAction(self.resultsAction)

        self.helpAction = QAction(QIcon(':/processing/images/help.png'),
                                     QCoreApplication.translate('Processing',
                                     '&LUMENS Help'),
                                     interface.iface.mainWindow())
        self.helpAction.triggered.connect(self.openHelp)
        self.menu.addAction(self.helpAction)

        self.statusLDBAction = QAction(QIcon(':/processing/images/database.png'),
                                     QCoreApplication.translate('Processing',
                                     '&Status LUMENS Database'),
                                     interface.iface.mainWindow())
        self.statusLDBAction.triggered.connect(self.openStatusLDB)   
        self.menu.addAction(self.helpAction)
         

        if hasattr(interface.iface, 'addDatabaseToolBarIcon'):
            interface.iface.addVectorToolBarIcon(self.statusLDBAction)
        else:
            interface.iface.addToolBarIcon(self.statusLDBAction)

        menuBar = interface.iface.mainWindow().menuBar()
        menuBar.insertMenu(interface.iface.firstRightStandardMenu().menuAction(), self.menu) #nanti dihide
        menuBar.insertMenu(interface.iface.firstRightStandardMenu().menuAction(), self.mDatabaseMenu)
        menuBar.insertMenu(interface.iface.firstRightStandardMenu().menuAction(), self.mPurMenu)
        menuBar.insertMenu(interface.iface.firstRightStandardMenu().menuAction(), self.mQuesMenu)
        menuBar.insertMenu(interface.iface.firstRightStandardMenu().menuAction(), self.mTaMenu)
        menuBar.insertMenu(interface.iface.firstRightStandardMenu().menuAction(), self.mSciendoMenu)
        menuBar.insertMenu(interface.iface.firstRightStandardMenu().menuAction(), self.mToolsMenu)

#         cara lain bikin menu lewat qgisapp.ui qgisapp.cpp:7403 qgisapp.h  
#         if hasattr(interface.iface, "addPluginToVectorMenu"):
#             interface.iface.addPluginToVectorMenu("TES", self.mElevationMenu)
#         else:
            # no menu, place plugin under Plugins menu and toolbox as usual
#             QMessageBox.warning(interface.iface.mainWindow(), QCoreApplication.translate('PUR', 'PUR'), QCoreApplication.translate('PUR', 'Ga bisa bro'))

        self.commanderAction = QAction(
                QIcon(':/processing/images/commander.png'),
                QCoreApplication.translate('Processing', '&Commander'),
                interface.iface.mainWindow())
        self.commanderAction.triggered.connect(self.openCommander)
        self.menu.addAction(self.commanderAction)
        interface.iface.registerMainWindowAction(self.commanderAction,
                'Ctrl+Alt+M')
        
        
    def unload(self):
        self.toolbox.setVisible(False)
        self.menu.deleteLater()
        self.mDatabaseMenu.deleteLater()
        self.mPurMenu.deleteLater()
        self.mQuesMenu.deleteLater()
        self.mTaMenu.deleteLater()
        self.mSciendoMenu.deleteLater()
        self.mToolsMenu.deleteLater()

        # delete temporary output files
        folder = tempFolder()
        if QDir(folder).exists():
            shutil.rmtree(folder, True)

        if hasattr( interface.iface, 'removeVectorToolBarIcon' ):
            interface.iface.removeVectorToolBarIcon(self.statusLDBAction)
        else:
            interface.iface.removeToolBarIcon(self.statusLDBAction)

        interface.iface.unregisterMainWindowAction(self.commanderAction)
        
    def openCommander(self):
        if self.commander is None:
            self.commander = CommanderWindow(interface.iface.mainWindow(),
                    interface.iface.mapCanvas())
            Processing.addAlgListListener(self.commander)
        self.commander.prepareGui()
        self.commander.show()

    def openToolbox(self):
        if self.toolbox.isVisible():
            self.toolbox.hide()
        else:
            self.toolbox.show()

    def openModeler(self):
        dlg = ModelerDialog()
        dlg.exec_()
        if dlg.update:
            Processing.updateAlgsList()
            self.toolbox.updateProvider('model')

    def openResults(self):
        dlg = ResultsDialog()
        dlg.exec_()

    def openStatusLDB(self):
        #=======================================================================
        # OPTION 1:
        # outputs = processing.runalg("r:lumensdatabasestatus", getTempFilename('csv'))
        # output = outputs['database_status']
        # dataobjects.load(output, 'table')
        #=======================================================================
        if not self.projectIsLoaded:
            QMessageBox.information(None, "Status LUMENS Database", "No LUMENS project loaded. Please create LUMENS project first.") 
            return                    
        
        #EXTRACT OUTPUT FROM R
        outputs = processing.runalg("r:lumensdatabasestatus", getTempFilename('csv'))
        output = outputs['database_status']
        try:
            original = file(output, 'rU')
        except Exception, e:
            return
        reader = csv.reader(original)
                
        #CREATE HTML FILE
        statusLDB = self.LUMENSProjectPath + "/status_LUMENS_database.html"
        f = open(statusLDB, 'w')
        html = '<!DOCTYPE html>\n'
        html += '<html>\n<head>\n<meta name="qrichtext" content="1" />\n'
        html += """<style>
                    table a:link {color: #666;font-weight: bold;text-decoration:none;}
                    table a:visited {color: #999999;font-weight:bold;text-decoration:none;}
                    table a:active, table a:hover { color: #bd5a35;text-decoration:underline;}
                    table {font-family:Arial, Helvetica, sans-serif;color:#666;font-size:12px;text-shadow: 1px 1px 0px #fff;background:#eaebec;margin:20px;border:#ccc 1px solid;-moz-border-radius:3px;-webkit-border-radius:3px;border-radius:3px;-moz-box-shadow: 0 1px 2px #d1d1d1;-webkit-box-shadow: 0 1px 2px #d1d1d1;box-shadow: 0 1px 2px #d1d1d1;}
                    table th {
                        padding:10px 25px 11px 25px;
                        border-top:1px solid #fafafa;
                        border-bottom:1px solid #e0e0e0;
                    
                        background: #ededed;
                        background: -webkit-gradient(linear, left top, left bottom, from(#ededed), to(#ebebeb));
                        background: -moz-linear-gradient(top,  #ededed,  #ebebeb);
                    }
                    table th:first-child { text-align: left; padding-left:20px; }
                    table tr:first-child th:first-child {-moz-border-radius-topleft:3px;-webkit-border-top-left-radius:3px;border-top-left-radius:3px;}
                    table tr:first-child th:last-child {-moz-border-radius-topright:3px;-webkit-border-top-right-radius:3px;border-top-right-radius:3px;}
                    table tr {text-align: center;padding-left:20px;}
                    table td:first-child {text-align: left;padding-left:20px;border-left: 0;}
                    table td {padding:5px;border-top: 1px solid #ffffff;border-bottom:1px solid #e0e0e0;border-left: 1px solid #e0e0e0;
                    
                        background: #fafafa;
                        background: -webkit-gradient(linear, left top, left bottom, from(#fbfbfb), to(#fafafa));
                        background: -moz-linear-gradient(top,  #fbfbfb,  #fafafa);
                    }
                    table tr.even td {background: #f6f6f6;background: -webkit-gradient(linear, left top, left bottom, from(#f8f8f8), to(#f6f6f6));background: -moz-linear-gradient(top,  #f8f8f8,  #f6f6f6);}
                    table tr:last-child td {border-bottom:0;}
                    table tr:last-child td:first-child {-moz-border-radius-bottomleft:3px;-webkit-border-bottom-left-radius:3px;border-bottom-left-radius:3px;}
                    table tr:last-child td:last-child {-moz-border-radius-bottomright:3px;-webkit-border-bottom-right-radius:3px;border-bottom-right-radius:3px;}
                    table tr:hover td {background: #f2f2f2;background: -webkit-gradient(linear, left top, left bottom, from(#f2f2f2), to(#f0f0f0));background: -moz-linear-gradient(top,  #f2f2f2,  #f0f0f0);}
                    </style>\n</head>\n<body>\n<table>\n"""
        count = 0
        for row in reader:
            if count == 0:
                html += '<tr><th>%s</th> <th>%s</th></tr>\n' %(row[1], row[2])
            else:
                html += '<tr><td>%s</td> <td>%s</td></tr>\n' %(row[1], row[2])
            count += 1
        html += '</table>\n</body>\n</html>'
        f.write(html)
        f.close
        
        #SHOW CURRENT STATUS OF LUMENS DATABASE
        self.status.setStatusDatabase(statusLDB)

    def openHistory(self):
        dlg = HistoryDialog()
        dlg.exec_()

    def openConfig(self):
        dlg = ConfigDialog(self.toolbox)
        dlg.exec_()

    def openHelp(self):
        if isWindows():
            if ProcessingConfig.getSetting(RUtils.R_USE64):
                webbrowser.open("C:/PROGRA~2/LUMENS/LUMENSHelp/index.html")
            else:
                webbrowser.open("C:/PROGRA~1/LUMENS/LUMENSHelp/index.html")
        #os.stat("/home/alfa/Desktop/LUMENSHelp/index.html")
                
    def openClipByMask(self):
        self.alg = ClipByMask()
        self.alg.provider = Providers.providers['gdalogr']
        dlg = ParametersDialog(self.alg)
        dlg.exec_()  
        
    def openReprojectVector(self):
        self.alg = ReprojectLayer()
        self.alg.provider = Providers.providers['qgis']
        dlg = ParametersDialog(self.alg)
        dlg.exec_()  
        
    def openReprojectRaster(self):
        self.alg = warp()
        self.alg.provider = Providers.providers['gdalogr']
        dlg = ParametersDialog(self.alg)
        dlg.exec_()  
        
    def openSlopeModel(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'slope_class_w_r.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()           
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("Tools", "Generate slope"), \
                QCoreApplication.translate("Tools","GRASS or R provider is not configured.\nPlease configure it before running this model."))  
            return          

    def openWatershedAnalysisModel(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'watershed_analysis_ver1.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()     
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("PUR", "Generate watershed boundary"), \
                QCoreApplication.translate("PUR","GRASS or R provider is not configured.\nPlease configure it before running this model."))  
            return    

    def openDistance(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'calculate_distance.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("Tools", "Calculate distance"), \
                QCoreApplication.translate("Tools","GRASS provider is not configured.\nPlease configure it before running this model."))  
            return     

    def openFixedDistanceBuffer(self):
        self.alg = FixedDistanceBuffer()
        self.alg.provider = Providers.providers['qgis']
        dlg = ParametersDialog(self.alg)
        dlg.exec_()  
        
    def openVariableDistanceBuffer(self):
        self.alg = VariableDistanceBuffer()
        self.alg.provider = Providers.providers['qgis']
        dlg = ParametersDialog(self.alg)
        dlg.exec_()     

    def openAbacus(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'redd_abacus_sp.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("Tools", "REDD Abacus SP"), \
                QCoreApplication.translate("Tools","R provider is not configured.\nPlease configure it before running this model."))  
            return     

    def openReclass(self):
        dlg = ReclassTable(interface.iface)
        dlg.exec_()    

    def openElevationModel(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'elev_class_w_r_2.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("Tools", "Classify"), \
                QCoreApplication.translate("Tools","R provider is not configured.\nPlease configure it before running this model."))  
            return            

    def openCombineModel(self):
        num = NumberCombine()
        num.exec_()
        
    def openReconcileOld(self):
        self.alg = Reconcile()
        self.alg.provider = Providers.providers['qgis']
        dlg = ParametersDialog(self.alg)
        dlg.exec_()  

    def openReconcile(self):
        folder = \
            os.path.join(os.path.dirname(RAlgorithmProvider.__file__),
                         'scripts')
        f = os.path.join(folder, 'PUR_reconcile_by_reference.rsx')
        try:
            m = RAlgorithm(f)
            self.alg = m
            self.alg.provider = Providers.providers['r']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()   
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("Tools", "Cross tabulation"), \
                QCoreApplication.translate("Tools","R provider is not configured.\nPlease configure it before running this model."))  
            return         
        '''
        layer = interface.iface.activeLayer()
#         layer = QgsVectorLayer("file:///home/alfa/Desktop/sample.csv","sample","delimitedtext")
        if layer == None or layer.type() != layer.VectorLayer:
            QMessageBox.warning(interface.iface.mainWindow(), QCoreApplication.translate('PUR', 'Modify attribute table'), QCoreApplication.translate('PUR', 'Please select a vector layer'))
        elif layer.isEditable():
            QMessageBox.warning(interface.iface.mainWindow(), QCoreApplication.translate('PUR','Modify attribute table'), QCoreApplication.translate('PUR','The selected layer is currently in editing mode.\nPlease exit this mode before managing the table.'))
        else:
            interface.iface.showAttributeTable(layer)
        '''

    def openCharacterize(self):
        layer = interface.iface.activeLayer()
        if layer == None or layer.type() != layer.VectorLayer:
            QMessageBox.warning(interface.iface.mainWindow(), QCoreApplication.translate('Tools', 'Area Statistic'), QCoreApplication.translate('PUR', 'Please select a vector layer'))
        elif layer.isEditable():
            QMessageBox.warning(interface.iface.mainWindow(), QCoreApplication.translate('Tools','Area Statistic'), QCoreApplication.translate('PUR','The selected layer is currently in editing mode.\nPlease exit this mode before managing the table.'))
        else:
#         dlg = Characterize(interface.iface)
#         dlg.exec_()
            dlg = TableManager(interface.iface)
            dlg.exec_()    
            
    def openZonalStatistics(self):
        self.alg = ZonalStatistics()
        self.alg.provider = Providers.providers['qgis']
        dlg = ParametersDialog(self.alg)
        dlg.exec_()   
                   
    def openCrossTab(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'cross_tabulation.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()   
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("Tools", "Cross tabulation"), \
                QCoreApplication.translate("Tools","R provider is not configured.\nPlease configure it before running this model."))  
            return         

    def openEditCrossTab(self):
        crossTab = EditCrossTabulation()
        crossTab.exec_()

    def openRasterize(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'rasterize.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()   
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("Tools", "Convert to raster"), \
                QCoreApplication.translate("Tools","GRASS provider is not configured.\nPlease configure it before running this model."))  
            return           

    def openPreQUESChange(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'Pre-QUES.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()   
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("QUES", "Land cover change analysis"), \
                QCoreApplication.translate("Tools","R provider is not configured.\nPlease configure it before running this model."))  
            return           

    def openLCC(self):
        folder = \
            os.path.join(os.path.dirname(RAlgorithmProvider.__file__),
                         'scripts')
        f = os.path.join(folder, 'LUMENS_Combined_PreQUES.rsx')
        try:
            m = RAlgorithm(f)
            self.alg = m
            self.alg.provider = Providers.providers['r']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("QUES", "Land cover change analysis"), \
                QCoreApplication.translate("QUES","R provider is not configured.\nPlease configure it before running this model."))  
            return                

    def openAppend(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'PUR_add_special_planning_unit.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("QUES", "Land cover change analysis"), \
                QCoreApplication.translate("QUES","R provider is not configured.\nPlease configure it before running this model."))  
            return                


    def openLCT(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'Pre-QUES_trajectory.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("QUES", "Land cover trajectories analysis"), \
                QCoreApplication.translate("QUES","R provider is not configured.\nPlease configure it before running this model."))  
            return                

    def openCarbon(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'QUES-C.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("QUES", "QUES-C"), \
                QCoreApplication.translate("QUES","R provider is not configured.\nPlease configure it before running this model."))  
            return     

    def openPeatCarbon(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'QUES-C_Peat.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("QUES", "Peatland carbon accounting"), \
                QCoreApplication.translate("QUES","R provider is not configured.\nPlease configure it before running this model."))  
            return     
        
    def openCarbonInteractive(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'QUES-C_Interactive.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("QUES", "QUES-C"), \
                QCoreApplication.translate("QUES","R provider is not configured.\nPlease configure it before running this model."))  
            return                

    def openSumMultiPeriod(self):
        folder = \
            os.path.join(os.path.dirname(RAlgorithmProvider.__file__),
                         'scripts')
        f = os.path.join(folder, 'Summarize_multiple_periode.rsx')
        try:
            m = RAlgorithm(f)
            self.alg = m
            self.alg.provider = Providers.providers['r']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("QUES", "Summarize multiple periode"), \
                QCoreApplication.translate("QUES","R provider is not configured.\nPlease configure it before running this model."))  
            return                

    def openGroupStats(self):
        dlg = GroupStatsDialog()
        warstwy = QgsMapLayerRegistry.instance().mapLayers()       
        
        listaWarstw = []
        for id in warstwy.keys():                                
            if warstwy[id].type()==0:
                listaWarstw.append((warstwy[id].name(), id))
        
        if len(listaWarstw) == 0 or len(warstwy) == 0:
            QMessageBox.information(None,QCoreApplication.translate('GroupStats','Information'), \
                QCoreApplication.translate('GroupStats','Vector layers not found'))
            return
        dlg.iface = interface.iface
        dlg.ustawWarstwy(listaWarstw)
        
        # show the dialog
        dlg.show()

    def openBiodiversity(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'QUES-B.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("QUES", "QUES-B Analysis"), \
                QCoreApplication.translate("QUES","R provider is not configured.\nPlease configure it before running this model."))  
            return

    def openAbacusOpCost(self):  
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'abacus_opportunity_cost.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate('TA','Opportunity cost'), \
                QCoreApplication.translate('TA','R provider is not configured.\nPlease configure it before running this model.'))   
            return

    def openOpCost(self):  
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'opportunity_cost.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate('TA','Opportunity cost'), \
                QCoreApplication.translate('TA','R provider is not configured.\nPlease configure it before running this model.'))   
            return
            
    def openOpCost2(self):  
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'opcost_map.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate('TA','Opportunity cost'), \
                QCoreApplication.translate('TA','R provider is not configured.\nPlease configure it before running this model.'))   
            return

    def openHistAnnualProj(self):
        folder = \
            os.path.join(os.path.dirname(RAlgorithmProvider.__file__),
                         'scripts')
        f = os.path.join(folder, 'Historical_Baseline_Annual_Projection.rsx')
        try:
            m = RAlgorithm(f)
            self.alg = m
            self.alg.provider = Providers.providers['r']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("SCIENDO", "Historical Baseline Annual Projection"), \
                QCoreApplication.translate("SCIENDO","R provider is not configured.\nPlease configure it before running this model."))  
            return 

    def openCalcHist(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'calculate_historical.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("SCIENDO", "Calculate projection historical baseline"), \
                QCoreApplication.translate("SCIENDO","R provider is not configured.\nPlease configure it before running this model."))  
            return 

    def openHistBaseline(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'projection_historical_baseline.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("SCIENDO", "Projection of Historical Baseline"), \
                QCoreApplication.translate("SCIENDO","R provider is not configured.\nPlease configure it before running this model."))  
            return 

    def openHistBaselineGUI(self):
        iter = IterationUI()
        iter.exec_()
        
    def openAbacusScenario(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'abacus_projection_scenario.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("SCIENDO", "Abacus forward looking BAU projection"), \
                QCoreApplication.translate("SCIENDO","R provider is not configured.\nPlease configure it before running this model.")) 
            return          

    def openAbacusAbsArea(self):
        folder = \
            os.path.join(os.path.dirname(RAlgorithmProvider.__file__),
                         'scripts')
        f = os.path.join(folder, 'Abacus_using_absolute_area.rsx')
        try:
            m = RAlgorithm(f)
            self.alg = m
            self.alg.provider = Providers.providers['r']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("SCIENDO", "Abacus using absolute area"), \
                QCoreApplication.translate("SCIENDO","R provider is not configured.\nPlease configure it before running this model."))  
            return 


    def openCalcEmissionReduction(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'Calculate_emission_reduction.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("SCIENDO", "Calculate emission reduction"), \
                QCoreApplication.translate("SCIENDO","R provider is not configured.\nPlease configure it before running this model.")) 
            return          

    def openCompareScenario(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'calculate_multiple_scenario.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("SCIENDO", "Compare scenario"), \
                QCoreApplication.translate("SCIENDO","R provider is not configured.\nPlease configure it before running this model.")) 
            return

    def openScenario(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'projection_scenario.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("SCIENDO", "Projection on Scenario"), \
                QCoreApplication.translate("SCIENDO","R provider is not configured.\nPlease configure it before running this model.")) 
            return
            
    def openDriversAnalysis(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'Drivers_analysis.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("SCIENDO", "Drivers analysis"), \
                QCoreApplication.translate("SCIENDO","R provider is not configured.\nPlease configure it before running this model.")) 
            return

    def openMolusce(self):
        d = MolusceDialog(interface.iface)
        d.show()
        d.exec_()
        
    def openDefCatchment(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'QUES-H_define_catchment_area.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("QUES", "Define Catchment Area"), \
                QCoreApplication.translate("QUES","R provider is not configured.\nPlease configure it before running this model.")) 
            return 

    def openLCFrac(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'QUES-H_LC_fraction.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("QUES", "Land cover fraction"), \
                QCoreApplication.translate("QUES","R provider is not configured.\nPlease configure it before running this model.")) 
            return
        
    def openHydroModel(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'QUES-H_hydrological_modeling.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("QUES", "Hydrological modeling"), \
                QCoreApplication.translate("QUES","R provider is not configured.\nPlease configure it before running this model.")) 
            return       
        
    def openWatershedInd(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'QUES-H_watershed_indicators.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("QUES", "Watershed indicators"), \
                QCoreApplication.translate("QUES","R provider is not configured.\nPlease configure it before running this model.")) 
            return

    def openWatershedModEval(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'QUES-H_watershed_model_evaluation.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("QUES", "Watershed model evaluation"), \
                QCoreApplication.translate("QUES","R provider is not configured.\nPlease configure it before running this model.")) 
            return

    def openWatershedDel(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'QUES-H_generate_basin.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("QUES", "Watershed delineation"), \
                QCoreApplication.translate("QUES","R provider is not configured.\nPlease configure it before running this model.")) 
            return
        
    def openRecRefClass(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'PUR_reconcile_by_reference_map.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg) 
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("PUR", "Reconcile using reference class"), \
                QCoreApplication.translate("PUR","R provider is not configured.\nPlease configure it before running this model.")) 
            return

    def openReconcileStep1(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'PUR_step1_create_reference_data.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg) 
            activeDlg = dlg.exec_()
            
            # after PUR step 1 is closed, running PUR step 2
            if not activeDlg:
                self.openReconcileStep2()
            
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("PUR", "Select reference data and prepare planning unit"), \
                QCoreApplication.translate("PUR","R provider is not configured.\nPlease configure it before running this model.")) 
            return
    
    def openReconcileStep2(self):
        folder = \
            os.path.join(os.path.dirname(RAlgorithmProvider.__file__),
                         'scripts')
        #f = os.path.join(folder, 'PUR_step2_prepare_planning_unit.model')
        f = os.path.join(folder, 'PUR-Step2_Prepare_planning_unit.rsx')
        try:
            m = RAlgorithm(f)
            self.alg = m
            self.alg.provider = Providers.providers['r']
            dlg = BatchProcessingDialog(self.alg) 
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("PUR", "Select reference data and prepare planning unit"), \
                QCoreApplication.translate("PUR","R provider is not configured.\nPlease configure it before running this model.")) 
            return 
        
    def openReconcileStep3(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'PUR_step3_reconcile_planning_unit.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg) 
            activeDlg = dlg.exec_()
            
            # after PUR step 3 is closed, running PUR step 4
            if not activeDlg:
                self.openReconcileStep4()
                            
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("PUR", "Conduct reconciliation"), \
                QCoreApplication.translate("PUR","R provider is not configured.\nPlease configure it before running this model.")) 
            return             

    def openReconcileStep4(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'PUR_step4_finalization.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg) 
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("PUR", "Conduct reconciliation"), \
                QCoreApplication.translate("PUR","R provider is not configured.\nPlease configure it before running this model.")) 
            return

    def openCombinePU(self):
        processing.runalg("r:combineplanningunit")
        
        
    def openDominantHRU(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'QUES-H_DHRU.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("QUES", "QUES-H Dominant HRU HRU Definition"), \
                QCoreApplication.translate("QUES","R provider is not configured.\nPlease configure it before running this model.")) 
            return                       
    
    def openDominantLUSSL(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'QUES-H_DLUSSL.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("QUES", "QUES-H Dominant Land Use, Soil, Slope HRU Definiton"), \
                QCoreApplication.translate("QUES","R provider is not configured.\nPlease configure it before running this model.")) 
            return                   
    
    def openMultipleHRU(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'QUES-H_MHRU.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("QUES", "QUES-H Multiple HRU Definiton"), \
                QCoreApplication.translate("QUES","R provider is not configured.\nPlease configure it before running this model.")) 
            return                  
        
    def openSingleIODA(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'TA_REG_IO_DA.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("TA", "Single I-O Descriptive Analysis"), \
                QCoreApplication.translate("TA","R provider is not configured.\nPlease configure it before running this model.")) 
            return                  
        
    def openLandDistLR(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'TA_REG_LD_LR.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("TA", "Land Distribution & Requirement Analysis"), \
                QCoreApplication.translate("TA","R provider is not configured.\nPlease configure it before running this model.")) 
            return                  
        
    def openFinalDemand(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'TA_REG_LUC_5A_LCC_FD.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("TA", "Final Demand Change Multiplier Analysis"), \
                QCoreApplication.translate("TA","R provider is not configured.\nPlease configure it before running this model.")) 
            return                  
        
    def openGDP(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'TA_REG_LUC_5A_LCC_GDP.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("TA", "GDP Change Multiplier Analysis"), \
                QCoreApplication.translate("TA","R provider is not configured.\nPlease configure it before running this model.")) 
            return                  
        
    def openImpactLU(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'TA_REG_LUC_GDP_LCC.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("TA", "Impact of Land Using to Regional Economy Indicator Analysis"), \
                QCoreApplication.translate("TA","R provider is not configured.\nPlease configure it before running this model.")) 
            return                  
        
    def openTimeSeriesIO(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'TA_REG_TS_IO.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("TA", " Time Series I-O Descriptive Analysis"), \
                QCoreApplication.translate("TA","R provider is not configured.\nPlease configure it before running this model.")) 
            return                           
    

#===============================================================================
#     def layerChanged(self):
#         self.layer = interface.iface.activeLayer()
# 
#         if self.layer is None:
#             return
# 
#         if self.layer.type() != QgsMapLayer.RasterLayer:
#             self.rasterSlider.disableOrEnableControls(False)
#             return
# 
#         if self.layer.providerType() not in ["gdal", "grass"]:
#             self.rasterSlider.disableOrEnableControls(False)
#             return
# 
#         if self.layer.bandCount() > 1 and self.layer.renderer().type() not in self.singleBandStyles:
#             self.rasterSlider.disableOrEnableControls(False)
#             return
# 
#         band = self.layer.renderer().usesBands()[0]
#         stat = self.layer.dataProvider().bandStatistics(band)
#         maxValue = int(stat.maximumValue)
#         minValue = int(stat.minimumValue)
#         self.rasterSlider.updateSliders(maxValue, minValue)
# 
#         self.rasterSlider.disableOrEnableControls(True)
#===============================================================================
    
    
    '''DINAMICA by AE'''    
    def openSCIENDOTCM(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'SCIENDO1_Calculate_transition_matrix.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("SCIENDO", "Calculate transition matrix"), \
                QCoreApplication.translate("SCIENDO","R provider is not configured.\nPlease configure it before running this model.")) 
            return

    def openSCIENDORC(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'SCIENDO1_Create_raster_cube.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("SCIENDO", "Create raster cube of factors"), \
                QCoreApplication.translate("SCIENDO","R provider is not configured.\nPlease configure it before running this model.")) 
            return
            
    def openSCIENDOWOE(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'SCIENDO3_Calculate_weight_of_evidence.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("SCIENDO", "Calculate weight of evidence"), \
                QCoreApplication.translate("SCIENDO","R provider is not configured.\nPlease configure it before running this model.")) 
            return

    def openSCIENDOSLU(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'SCIENDO4_Simulate_land_use_change.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("SCIENDO", "Simulate land use change"), \
                QCoreApplication.translate("SCIENDO","R provider is not configured.\nPlease configure it before running this model.")) 
            return

    def openSCIENDOSLU2(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'SCIENDO5_Simulate_with_scenario.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("SCIENDO", "Simulate with scenario"), \
                QCoreApplication.translate("SCIENDO","R provider is not configured.\nPlease configure it before running this model.")) 
            return  

    def readLUMENSLogFile(self):
        self.LUMENSLogFile = os.path.join(QgsApplication.systemEnvVars()['PUBLIC'], "LUMENS\\LUMENS.log")
        try:
            if os.stat(self.LUMENSLogFile).st_size != 0:
                lines = open(self.LUMENSLogFile).read().split(',')
                self.LUMENSProjectPath = lines[0] + '/' + lines[1]
                self.activeProject.clear()
                self.activeProject.addAction("Active project: %s" % self.LUMENSProjectPath)  #interface.iface.mainWindow().statusBar().showMessage( "Active project: %s" % self.LUMENSProjectPath )
                self.projectIsLoaded = True
                self.mCloseLDBMenu.setEnabled(True)
            else:
                self.activeProject.clear()
                self.activeProject.addAction("No active project loaded")  #interface.iface.mainWindow().statusBar().showMessage( "No active project loaded" )
                self.projectIsLoaded = False
                self.mCloseLDBMenu.setEnabled(False)
        except Exception, e:
            pass
            #self.activeProject.addAction("No active project created in LUMENS, create new project first") 
            #interface.iface.mainWindow().statusBar().showMessage( "No active project created in LUMENS. Create new project first." )        
        
    def openCreateLDB(self): 
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'LUMENS_create_database.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg) 
            dlg.exec_()
            self.readLUMENSLogFile()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("Database", "Create LUMENS Database"), \
                QCoreApplication.translate("Database","R provider is not configured.\nPlease configure it before running this model.")) 
            return
            
    def openOpenLDB(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'LUMENS_open_database.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg) 
            dlg.exec_()
            self.readLUMENSLogFile()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("Database", "Open LUMENS Database"), \
                QCoreApplication.translate("Database","R provider is not configured.\nPlease configure it before running this model.")) 
            return
            
    def openCloseLDB(self):
        processing.runalg("modeler:lumens_close_database")
        self.readLUMENSLogFile()

    def openDelData(self):
        processing.runalg("r:lumensdeletedata")

    def openImportLDB(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'LUMENS_import_database.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg) 
            dlg.exec_()
            self.readLUMENSLogFile()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("Database", "Import LUMENS Database"), \
                QCoreApplication.translate("Database","R provider is not configured.\nPlease configure it before running this model.")) 
            return
            
    def openRasterLanduseLDB(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'LUMENS_add_landcover_raster.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg) 
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("Database", "Add land use/cover data"), \
                QCoreApplication.translate("Database","R provider is not configured.\nPlease configure it before running this model.")) 
            return

    def openRasterPeatLDB(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'LUMENS_add_peat.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg) 
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("Database", "Add peat data"), \
                QCoreApplication.translate("Database","R provider is not configured.\nPlease configure it before running this model.")) 
            return            
            
    def openRasterFactorLDB(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'LUMENS_add_factor_data.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg) 
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("Database", "Add factor data"), \
                QCoreApplication.translate("Database","R provider is not configured.\nPlease configure it before running this model.")) 
            return

            
    def openRasterPlanningUnitLDB(self):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'LUMENS_add_planning_unit.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg) 
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("Database", "Add planning unit data"), \
                QCoreApplication.translate("Database","R provider is not configured.\nPlease configure it before running this model.")) 
            return
        
        
class IterationUI(QtGui.QDialog):
     
    def __init__(self):
        QtGui.QDialog.__init__(self)
                 
        self.resize(200, 100)
        self.verticalLayout = QtGui.QVBoxLayout(self)
        self.spin_iter = QtGui.QSpinBox(self)
        self.spin_iter.setRange(1, 10)
        self.spin_iter.setValue(5)
        
        self.verticalLayout.addWidget(self.spin_iter)
        self.buttonBox = QtGui.QDialogButtonBox(self)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName('buttonBox')
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL('accepted()'),
                        self.okPressed)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL('rejected()'),
                        self.cancelPressed)
         
        self.verticalLayout.addWidget(self.buttonBox)
        self.setWindowTitle('Iteration')
        self.show()
         
    def okPressed(self):
        processing.runalg("modeler:projection_historical_baseline_gui", self.spin_iter.value())
        self.close()
             
    def cancelPressed(self):
        self.close()
