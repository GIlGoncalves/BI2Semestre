USE [LEI]
GO

/****** Object:  Table [dbo].[DataOrdem]    Script Date: 12/03/2017 15:24:14 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[DataOrdem](
	[id] [int] IDENTITY(1,1) NOT NULL,
	[Data] [datetime] NOT NULL,
	[Mes] [int] NOT NULL,
	[Ano] [int] NOT NULL,
	[Semana] [int] NOT NULL,
	[Dia] [int] NOT NULL,
 CONSTRAINT [PK_DataOrdem] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO


