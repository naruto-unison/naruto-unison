module Handler.AdminSpec (spec) where

import Import

spec :: Spec
spec = withApp do
    describe "AdminR" do
        it "responds 403 if not admin" do
            authenticateAs =<< createUser Normal "dummy"
            get AdminR
            statusIs 403
        it "responds 200 if admin" do
            authenticateAs =<< createUser Admin "dummy"
            get AdminR
            statusIs 200

    describe "UsageR" do
        it "responds 403 if not admin" do
            authenticateAs =<< createUser Normal "dummy"
            get UsageR
            statusIs 403
        it "responds 200 if admin" do
            authenticateAs =<< createUser Admin "dummy"
            get UsageR
            statusIs 200
