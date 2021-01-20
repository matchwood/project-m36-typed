
module ProjectM36.Typed (
    module ProjectM36.Typed.Wrapped,
    module ProjectM36.Typed.Internal,
    module ProjectM36.Typed.Ops,

    -- exporting from Execute
    -- errors
    E.DbErrorQ(..),
    E.ToDbErrorQ(..),
    E.RollbackException(..),

    -- database interaction
    E.DbConnection(..),
    E.HasDbConnection(..),

    -- concrete implementations
    E.QueryM,
    E.executeQueryM,
    E.UpdateM,
    E.executeUpdateM,
    -- environment
    E.HasCurrentTime(..),
    E.GetCurrentTimeM(..),

    -- utilities
    E.liftEitherQ,
    E.throwQ,


    -- exporting m36
    C.ConnectionInfo(..),
    C.PersistenceStrategy(..),
    C.emptyNotificationCallback,
    C.Tupleable(..),
    C.Atomable(..),
    C.Atom(..),
    C.Attributes,
    C.RelationTuple(..),
    C.Attribute(..),
    C.AtomType(..),
    C.DatabaseContextExpr,
    C.atomForAttributeName,


    -- this file

    connectProjectM36T,
    closeDb,


    ) where

import RIO

import ProjectM36.Base as C
import ProjectM36.Client as C
import ProjectM36.Tupleable as C
import ProjectM36.Tuple as C

import ProjectM36.Typed.Ops
import ProjectM36.Typed.Internal
import ProjectM36.Typed.Wrapped
import ProjectM36.Typed.Execute as E




connectProjectM36T :: LogFunc -> ConnectionInfo -> QDbSchema db -> IO (Either DbErrorQ (DbConnection db))
connectProjectM36T lf ci sc = do
   eConn <- connectProjectM36 ci
   case eConn of
    Left e -> return . Left $ toDbErrorQ e
    Right conn -> do
      eS <- createSessionAtHead conn "master"
      case eS of
        Left e -> return . Left $ toDbErrorQ e
        Right s -> do
          res <- runRIO lf $ createSchema s conn sc
          case res of
            Left errs -> return . Left $ toDbErrorQ errs
            Right dbConn -> return . Right $ dbConn


closeDb :: DbConnection db -> IO ()
closeDb DbConnection{..} = closeSession dbSession dbConnection >> close dbConnection


