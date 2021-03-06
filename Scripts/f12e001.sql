if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[F12E001]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[F12E001]
GO

CREATE TABLE [dbo].[F12E001] (
	[F12ECODPRO] [char] (6) COLLATE Modern_Spanish_CI_AS NOT NULL ,
	[F12ECODART] [char] (13) COLLATE Modern_Spanish_CI_AS NOT NULL ,
	[F12ECODTAL] [char] (5) COLLATE Modern_Spanish_CI_AS NULL ,
	[F12ECODCOL] [char] (5) COLLATE Modern_Spanish_CI_AS NULL ,
	[F12EPRIORI] [numeric](18, 0) NULL ,
	[F12ECODUBI] [char] (14) COLLATE Modern_Spanish_CI_AS NULL ,
	[F12ESTKMIN] [numeric](18, 0) NULL ,
	[F12ESTKMAX] [numeric](18, 0) NULL 
) ON [PRIMARY]
GO

