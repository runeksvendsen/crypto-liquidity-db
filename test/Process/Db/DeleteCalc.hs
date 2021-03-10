module Process.Db.DeleteCalc
( deleteCalculations
)
where

import Database ( liquidityDb, LiquidityDb(calculations) )
import qualified Schema.Calculation as Calc

import Database.Beam
    ( SqlDelete, delete, SqlValable(val_), SqlIn(in_), runDelete, MonadBeam )
import Database.Beam.Backend ( SqlSerial, BeamSqlBackend )


deleteCalculations :: (MonadBeam be m, BeamSqlBackend be) => [SqlSerial Calc.Int32] -> m ()
deleteCalculations idList = runDelete $
    delete (calculations liquidityDb)
           (\c -> Calc.calculationId c `in_` map val_ idList)
